package com.safetyfire.monitor.service;

import cn.hutool.json.JSONUtil;
import com.safetyfire.monitor.domain.entity.ProvincialWarningEntity;
import com.safetyfire.monitor.domain.entity.ReportTaskEntity;
import com.safetyfire.monitor.mapper.ReportTaskMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestClient;

import java.time.Duration;

/**
 * 省厅对接调度：
 * - 拉取预警（可选启用）
 * - 发送反馈（重试）
 */
@Component
public class ProvincialScheduler {
    private static final Logger log = LoggerFactory.getLogger(ProvincialScheduler.class);

    private final boolean enabled;
    private final String baseUrl;
    private final String warningPath;
    private final String feedbackPath;
    private final ProvincialService provincialService;
    private final ReportTaskMapper reportTaskMapper;
    private final RestClient restClient;

    public ProvincialScheduler(
            @Value("${app.provincial.enabled}") boolean enabled,
            @Value("${app.provincial.base-url}") String baseUrl,
            @Value("${app.provincial.warning-page-path}") String warningPath,
            @Value("${app.provincial.feedback-path}") String feedbackPath,
            ProvincialService provincialService,
            ReportTaskMapper reportTaskMapper
    ) {
        this.enabled = enabled;
        this.baseUrl = baseUrl;
        this.warningPath = warningPath;
        this.feedbackPath = feedbackPath;
        this.provincialService = provincialService;
        this.reportTaskMapper = reportTaskMapper;
        this.restClient = RestClient.builder().baseUrl(baseUrl).build();
    }

    /**
     * 拉取预警：由于省厅接口字段差异较大，这里仅演示把原始 JSON 入库。
     * 生产：请按省厅真实字段解析 externalId/riskLevel/pushType/time/companyCode。
     */
    @Scheduled(fixedDelay = 120_000)
    public void syncWarnings() {
        if (!enabled) return;
        try {
            String raw = restClient.get().uri(warningPath).retrieve().body(String.class);
            if (raw == null || raw.isBlank()) return;

            // 兜底写入一条“同步记录”，便于验证链路（真实对接需解析列表并 upsert 多条）
            ProvincialWarningEntity e = new ProvincialWarningEntity();
            e.setExternalId("SYNC_" + System.currentTimeMillis());
            e.setCompanyCode(null);
            e.setRiskLevel(null);
            e.setPushType(null);
            e.setWarningTime(System.currentTimeMillis());
            e.setRawJson(raw);
            provincialService.upsertWarning(e);
        } catch (Exception ex) {
            log.warn("省厅预警拉取失败 baseUrl={} path={} err={}", baseUrl, warningPath, ex.getMessage());
        }
    }

    @Scheduled(fixedDelay = 10_000)
    public void sendFeedbackTasks() {
        if (!enabled) return;
        long now = System.currentTimeMillis();
        for (ReportTaskEntity task : reportTaskMapper.listDueTasks(ProvincialService.TASK_FEEDBACK, now, 20)) {
            if (reportTaskMapper.markRunning(task.getId()) <= 0) continue;
            try {
                String payload = task.getPayloadJson();
                restClient.post()
                        .uri(feedbackPath)
                        .contentType(org.springframework.http.MediaType.APPLICATION_JSON)
                        .body(payload == null ? "{}" : payload)
                        .retrieve()
                        .toBodilessEntity();
                reportTaskMapper.markSuccess(task.getId());
            } catch (Exception ex) {
                int retry = (task.getRetryCount() == null ? 0 : task.getRetryCount()) + 1;
                long backoff = Math.min(300_000L, (long) Math.pow(2, Math.min(10, retry)) * 1000L);
                long next = now + backoff;
                reportTaskMapper.markFailed(task.getId(), ex.getMessage(), retry, next);
                log.warn("省厅反馈发送失败 taskId={} retry={} next={} payload={} err={}", task.getId(), retry, next,
                        JSONUtil.toJsonStr(task.getBizKey()), ex.getMessage());
            }
        }
    }
}
