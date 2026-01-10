package com.safetyfire.monitor.service;

import cn.hutool.core.util.IdUtil;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.safetyfire.monitor.domain.dto.AiBoxHeartbeatRequest;
import com.safetyfire.monitor.domain.entity.AiBoxAlarmActionIngestEntity;
import com.safetyfire.monitor.domain.entity.AiBoxDeviceEntity;
import com.safetyfire.monitor.domain.entity.AiBoxMediaEntity;
import com.safetyfire.monitor.mapper.AiBoxAlarmActionIngestMapper;
import com.safetyfire.monitor.mapper.AiBoxDeviceMapper;
import com.safetyfire.monitor.mapper.AiBoxMediaMapper;
import com.safetyfire.monitor.util.AiBoxTimeParser;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Objects;

/**
 * AI盒子 HTTP 推送接入服务：
 * - 登录/心跳/行为告警原始数据入库
 * - 图片/录像上传落盘 + file_object 入库 + ai_box_media 关联
 */
@Service
public class AiBoxHttpPushService {
    private final AiBoxDeviceMapper deviceMapper;
    private final AiBoxAlarmActionIngestMapper alarmActionMapper;
    private final AiBoxMediaMapper mediaMapper;
    private final AiBoxMediaStorageService mediaStorageService;
    private final ObjectMapper objectMapper;
    private final AiBoxAlarmService aiBoxAlarmService;

    public AiBoxHttpPushService(AiBoxDeviceMapper deviceMapper,
                                AiBoxAlarmActionIngestMapper alarmActionMapper,
                                AiBoxMediaMapper mediaMapper,
                                AiBoxMediaStorageService mediaStorageService,
                                ObjectMapper objectMapper,
                                AiBoxAlarmService aiBoxAlarmService) {
        this.deviceMapper = deviceMapper;
        this.alarmActionMapper = alarmActionMapper;
        this.mediaMapper = mediaMapper;
        this.mediaStorageService = mediaStorageService;
        this.objectMapper = objectMapper;
        this.aiBoxAlarmService = aiBoxAlarmService;
    }

    @Transactional
    public String login(String deviceSerial, String ip) {
        String token = IdUtil.fastSimpleUUID();
        long now = System.currentTimeMillis();

        AiBoxDeviceEntity exists = deviceMapper.findByDeviceSerial(deviceSerial);
        if (exists == null) {
            AiBoxDeviceEntity e = new AiBoxDeviceEntity();
            e.setDeviceSerial(deviceSerial);
            e.setLoginToken(token);
            e.setLastLoginTimeMs(now);
            e.setLastHeartbeatTimeMs(null);
            e.setLastIp(ip);
            deviceMapper.insert(e);
        } else {
            deviceMapper.updateLogin(deviceSerial, token, now, ip);
        }
        return token;
    }

    @Transactional
    public void heartbeat(AiBoxHeartbeatRequest req, String ip) {
        if (req == null) return;
        String serial = req.deviceSerial();
        if (serial == null || serial.isBlank()) return;
        long now = System.currentTimeMillis();

        AiBoxDeviceEntity exists = deviceMapper.findByDeviceSerial(serial);
        if (exists == null) {
            AiBoxDeviceEntity e = new AiBoxDeviceEntity();
            e.setDeviceSerial(serial);
            e.setLoginToken(null);
            e.setLastLoginTimeMs(null);
            e.setLastHeartbeatTimeMs(now);
            e.setLastIp(ip);
            deviceMapper.insert(e);
        } else {
            deviceMapper.updateHeartbeat(serial, now, ip);
        }
    }

    /**
     * 行为告警推送：将原始 JSON 留痕，并尽力解析为系统内 AI 盒子告警（ai_box_alarm）。
     */
    @Transactional
    public Long ingestAlarmAction(String rawJson) {
        long receiveMs = System.currentTimeMillis();
        String deviceSerial = null;
        String alarmType = null;
        Long alarmTimeMs = null;

        try {
            JsonNode root = objectMapper.readTree(Objects.requireNonNullElse(rawJson, "{}"));
            deviceSerial = textFirst(root, "deviceSerial", "device_serial", "boxSerial", "serial");
            alarmType = textFirst(root, "alarmType", "alarm_type", "actionType", "eventType", "type");
            alarmTimeMs = timeFirst(root, "alarmTime", "warningTime", "warning_time", "time", "eventTime");
        } catch (Exception ignore) {
        }
        if (alarmTimeMs == null) alarmTimeMs = receiveMs;

        AiBoxAlarmActionIngestEntity e = new AiBoxAlarmActionIngestEntity();
        e.setDeviceSerial(deviceSerial);
        e.setAlarmType(alarmType);
        e.setAlarmTimeMs(alarmTimeMs);
        e.setRawPayload(Objects.requireNonNullElse(rawJson, "{}"));
        alarmActionMapper.insert(e);

        // 尝试映射为系统内 ai_box_alarm（触发统一告警事件），字段缺失则忽略
        tryIngestToAiBoxAlarm(rawJson, alarmType, alarmTimeMs);

        return e.getId();
    }

    /**
     * 图片/录像上传：写入 file_object，并记录 ai_box_media。
     */
    @Transactional
    public AiBoxMediaStorageService.StoredFile ingestMedia(String deviceSerial,
                                                           Long alarmActionId,
                                                           String mediaType,
                                                           String originalUrl,
                                                           String originalName,
                                                           String contentType,
                                                           byte[] bytes) {
        String biz = "IMAGE".equalsIgnoreCase(mediaType) ? "ai_box_image" : "ai_box_video";
        var stored = mediaStorageService.store(biz, originalName, contentType, bytes);

        AiBoxMediaEntity me = new AiBoxMediaEntity();
        me.setDeviceSerial(deviceSerial);
        me.setAlarmActionId(alarmActionId);
        me.setMediaType(mediaType);
        me.setOriginalUrl(originalUrl);
        me.setFileObjectId(stored.fileObjectId());
        me.setPublicUrl(stored.publicUrl());
        me.setSha256(stored.sha256());
        mediaMapper.insert(me);

        return stored;
    }

    private void tryIngestToAiBoxAlarm(String rawJson, String alarmType, Long alarmTimeMs) {
        try {
            JsonNode root = objectMapper.readTree(Objects.requireNonNullElse(rawJson, "{}"));
            String status = textFirst(root, "alarmStatus", "status");
            if (status == null || status.isBlank()) status = "ACTIVE";
            String cameraCode = textFirst(root, "cameraCode", "camera_code", "deviceCode", "device_code");
            String areaCode = textFirst(root, "areaCode", "area_code");
            Double height = doubleFirst(root, "height");
            Double weight = doubleFirst(root, "weight");
            String desc = textFirst(root, "description", "remark", "message");

            if (alarmType == null || alarmType.isBlank()) return;
            if (alarmTimeMs == null) return;

            aiBoxAlarmService.ingest(new com.safetyfire.monitor.domain.dto.AiBoxAlarmIngestRequest(
                    alarmType, status, alarmTimeMs, cameraCode, areaCode, height, weight, desc, null
            ));
        } catch (Exception ignore) {
        }
    }

    private static String textFirst(JsonNode root, String... keys) {
        if (root == null || keys == null) return null;
        for (String k : keys) {
            JsonNode n = root.get(k);
            if (n == null || n.isNull()) continue;
            String v = n.asText(null);
            if (v != null && !v.isBlank()) return v.trim();
        }
        return null;
    }

    private static Long timeFirst(JsonNode root, String... keys) {
        if (root == null || keys == null) return null;
        for (String k : keys) {
            JsonNode n = root.get(k);
            if (n == null || n.isNull()) continue;
            Long v = AiBoxTimeParser.tryParseToEpochMs(n.isNumber() ? n.numberValue() : n.asText(null));
            if (v != null) return v;
        }
        return null;
    }

    private static Double doubleFirst(JsonNode root, String... keys) {
        if (root == null || keys == null) return null;
        for (String k : keys) {
            JsonNode n = root.get(k);
            if (n == null || n.isNull()) continue;
            if (n.isNumber()) return n.doubleValue();
            String s = n.asText(null);
            if (s == null || s.isBlank()) continue;
            try {
                return Double.parseDouble(s.trim());
            } catch (NumberFormatException ignore) {
            }
        }
        return null;
    }
}

