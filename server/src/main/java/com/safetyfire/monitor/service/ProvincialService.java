package com.safetyfire.monitor.service;

import cn.hutool.json.JSONUtil;
import com.safetyfire.monitor.common.PageResponse;
import com.safetyfire.monitor.domain.dto.ProvincialFeedbackRequest;
import com.safetyfire.monitor.domain.entity.ProvincialWarningEntity;
import com.safetyfire.monitor.domain.entity.ProvincialWarningFeedbackEntity;
import com.safetyfire.monitor.domain.entity.ReportTaskEntity;
import com.safetyfire.monitor.mapper.ProvincialWarningFeedbackMapper;
import com.safetyfire.monitor.mapper.ProvincialWarningMapper;
import com.safetyfire.monitor.mapper.ReportTaskMapper;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

/**
 * 省厅对接服务（FR-07/FR-10）。
 */
@Service
public class ProvincialService {
    public static final String TASK_FEEDBACK = "PROVINCIAL_FEEDBACK";

    private final ProvincialWarningMapper warningMapper;
    private final ProvincialWarningFeedbackMapper feedbackMapper;
    private final ReportTaskMapper reportTaskMapper;

    public ProvincialService(ProvincialWarningMapper warningMapper, ProvincialWarningFeedbackMapper feedbackMapper, ReportTaskMapper reportTaskMapper) {
        this.warningMapper = warningMapper;
        this.feedbackMapper = feedbackMapper;
        this.reportTaskMapper = reportTaskMapper;
    }

    public PageResponse<ProvincialWarningEntity> listWarnings(int page, int pageSize) {
        int offset = (page - 1) * pageSize;
        List<ProvincialWarningEntity> list = warningMapper.list(offset, pageSize);
        long total = warningMapper.count();
        return new PageResponse<>(list, page, pageSize, total);
    }

    public PageResponse<ProvincialWarningFeedbackEntity> listFeedback(int page, int pageSize) {
        int offset = (page - 1) * pageSize;
        List<ProvincialWarningFeedbackEntity> list = feedbackMapper.list(offset, pageSize);
        long total = feedbackMapper.count();
        return new PageResponse<>(list, page, pageSize, total);
    }

    @Transactional
    public void submitFeedback(ProvincialFeedbackRequest req) {
        ProvincialWarningFeedbackEntity e = new ProvincialWarningFeedbackEntity();
        e.setExternalId(req.externalId());
        e.setFeedback(req.feedback());
        e.setStatus("PENDING");
        e.setLastError(null);
        feedbackMapper.insert(e);

        ReportTaskEntity task = new ReportTaskEntity();
        task.setTaskType(TASK_FEEDBACK);
        task.setBizKey(req.externalId());
        task.setPayloadJson(JSONUtil.toJsonStr(req));
        task.setStatus("PENDING");
        task.setRetryCount(0);
        task.setNextRetryTime(null);
        task.setLastError(null);
        reportTaskMapper.insertIgnore(task);
    }

    /**
     * 对外：写入省厅预警（对接同步或手工导入）。
     */
    @Transactional
    public void upsertWarning(ProvincialWarningEntity e) {
        warningMapper.upsert(e);
    }
}

