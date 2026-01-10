package com.safetyfire.monitor.domain.entity;

/**
 * AI盒子行为告警入库记录（原始 JSON 留痕）。
 */
public class AiBoxAlarmActionIngestEntity {
    private Long id;
    private String deviceSerial;
    private String alarmType;
    private Long alarmTimeMs;
    private String rawPayload;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getDeviceSerial() {
        return deviceSerial;
    }

    public void setDeviceSerial(String deviceSerial) {
        this.deviceSerial = deviceSerial;
    }

    public String getAlarmType() {
        return alarmType;
    }

    public void setAlarmType(String alarmType) {
        this.alarmType = alarmType;
    }

    public Long getAlarmTimeMs() {
        return alarmTimeMs;
    }

    public void setAlarmTimeMs(Long alarmTimeMs) {
        this.alarmTimeMs = alarmTimeMs;
    }

    public String getRawPayload() {
        return rawPayload;
    }

    public void setRawPayload(String rawPayload) {
        this.rawPayload = rawPayload;
    }
}

