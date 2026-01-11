package com.safetyfire.monitor.service;

import com.safetyfire.monitor.common.PageResponse;
import com.safetyfire.monitor.domain.entity.HardwareIngestLogEntity;
import com.safetyfire.monitor.mapper.HardwareIngestLogMapper;
import com.safetyfire.monitor.security.DataScopeService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * 硬件上报日志服务：记录原始上报 payload，便于后台排查“现场数据为何没入库/没触发告警”。
 */
@Service
public class HardwareIngestLogService {
    private static final Logger log = LoggerFactory.getLogger(HardwareIngestLogService.class);

    private final HardwareIngestLogMapper mapper;
    private final DataScopeService dataScopeService;

    public HardwareIngestLogService(HardwareIngestLogMapper mapper, DataScopeService dataScopeService) {
        this.mapper = mapper;
        this.dataScopeService = dataScopeService;
    }

    public void record(String channel, String topic, String messageType, String apiKey, String companyCode,
                       String payload, boolean ok, String errorMessage) {
        try {
            mapper.insert(buildEntity(channel, topic, messageType, apiKey, companyCode, payload, ok, errorMessage));
        } catch (Exception ex) {
            // 日志写入失败不影响主链路（避免硬件数据丢失）
            log.warn("硬件上报日志写入失败 err={}", ex.getMessage());
        }
    }

    /**
     * 严格模式：写入失败则抛出异常（用于对接排查阶段，避免“返回 200 但实际上没入库”的误导）。
     */
    public void recordStrict(String channel, String topic, String messageType, String apiKey, String companyCode,
                             String payload, boolean ok, String errorMessage) {
        mapper.insert(buildEntity(channel, topic, messageType, apiKey, companyCode, payload, ok, errorMessage));
    }

    public PageResponse<HardwareIngestLogEntity> list(String channel, String messageType, String apiKey, String companyCode,
                                                      Integer ok, String topicLike, int page, int pageSize) {
        int offset = (page - 1) * pageSize;
        List<String> scope = dataScopeService.currentCompanyCodesOrAll();
        List<HardwareIngestLogEntity> list = mapper.list(scope, channel, messageType, apiKey, companyCode, ok, topicLike, offset, pageSize);
        long total = mapper.count(scope, channel, messageType, apiKey, companyCode, ok, topicLike);
        return new PageResponse<>(list, page, pageSize, total);
    }

    private static String trimTo(String s, int maxLen) {
        if (s == null) return null;
        String v = s.trim();
        if (v.length() <= maxLen) return v;
        return v.substring(0, maxLen);
    }

    private static HardwareIngestLogEntity buildEntity(String channel, String topic, String messageType, String apiKey, String companyCode,
                                                       String payload, boolean ok, String errorMessage) {
        HardwareIngestLogEntity e = new HardwareIngestLogEntity();
        e.setIngestChannel(trimTo(channel, 16));
        e.setTopic(trimTo(topic, 255));
        e.setMessageType(trimTo(messageType, 64));
        e.setApiKey(trimTo(apiKey, 64));
        e.setCompanyCode(trimTo(companyCode, 64));
        e.setPayload(trimTo(payload, 20000)); // 避免异常大 payload 撑爆数据库
        e.setParsedOk(ok ? 1 : 0);
        e.setErrorMessage(trimTo(errorMessage, 512));
        e.setReceiveTimeMs(System.currentTimeMillis());
        return e;
    }
}
