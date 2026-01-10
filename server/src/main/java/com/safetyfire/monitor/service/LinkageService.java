package com.safetyfire.monitor.service;

import com.safetyfire.monitor.common.PageResponse;
import com.safetyfire.monitor.domain.dto.LinkageCreateRequest;
import com.safetyfire.monitor.domain.entity.LinkageEventEntity;
import com.safetyfire.monitor.mapper.LinkageEventMapper;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

/**
 * 联动服务（FR-11）：音柱/TTS 真实协议为 MQTT（MQTTX）。
 */
@Service
public class LinkageService {
    private final LinkageEventMapper mapper;
    private final MqttPublisher mqttPublisher;

    public LinkageService(LinkageEventMapper mapper, MqttPublisher mqttPublisher) {
        this.mapper = mapper;
        this.mqttPublisher = mqttPublisher;
    }

    @Transactional
    public Long create(LinkageCreateRequest req) {
        LinkageEventEntity e = new LinkageEventEntity();
        e.setAlarmId(req.alarmId());
        e.setLinkageType(req.linkageType());
        e.setTarget(req.target());
        e.setPayload(req.payload());
        e.setStatus("PENDING");
        e.setLastError(null);
        mapper.insert(e);
        return e.getId();
    }

    public PageResponse<LinkageEventEntity> list(String status, int page, int pageSize) {
        int offset = (page - 1) * pageSize;
        List<LinkageEventEntity> list = mapper.list(offset, pageSize, status);
        long total = mapper.count(status);
        return new PageResponse<>(list, page, pageSize, total);
    }

    /**
     * 执行一条联动：
     * - MQTT_TTS：target=topic 或 topic|qos（如 tts/topic|1），payload 为 TTS 文本
     *
     * 兼容：历史上曾把音柱称作 “IP_TTS”，但真实协议是 MQTTX，因此 IP_TTS 等同 MQTT_TTS。
     */
    public void execute(LinkageEventEntity e) throws Exception {
        if ("MQTT_TTS".equalsIgnoreCase(e.getLinkageType()) || "IP_TTS".equalsIgnoreCase(e.getLinkageType())) {
            String target = e.getTarget() == null ? "" : e.getTarget().trim();
            String topic = target;
            int qos = 1;
            int idx = target.lastIndexOf('|');
            if (idx > 0) {
                topic = target.substring(0, idx).trim();
                qos = Integer.parseInt(target.substring(idx + 1).trim());
            }
            mqttPublisher.publish(topic, e.getPayload(), qos);
            return;
        }
        // 其他类型暂留空：生产可扩展为 HTTP Webhook、短信、邮件等
        throw new IllegalArgumentException("不支持的联动类型：" + e.getLinkageType());
    }

    @Transactional
    public void markSent(Long id) {
        mapper.markSent(id);
    }

    @Transactional
    public void markFailed(Long id, String err) {
        mapper.markFailed(id, err);
    }

    public List<LinkageEventEntity> pickPending(int limit) {
        return mapper.listPending(limit);
    }
}
