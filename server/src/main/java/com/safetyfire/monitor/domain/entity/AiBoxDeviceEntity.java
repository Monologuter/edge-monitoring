package com.safetyfire.monitor.domain.entity;

/**
 * AI盒子设备实体（HTTP推送接入）。
 */
public class AiBoxDeviceEntity {
    private Long id;
    private String deviceSerial;
    private String loginToken;
    private Long lastLoginTimeMs;
    private Long lastHeartbeatTimeMs;
    private String lastIp;

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

    public String getLoginToken() {
        return loginToken;
    }

    public void setLoginToken(String loginToken) {
        this.loginToken = loginToken;
    }

    public Long getLastLoginTimeMs() {
        return lastLoginTimeMs;
    }

    public void setLastLoginTimeMs(Long lastLoginTimeMs) {
        this.lastLoginTimeMs = lastLoginTimeMs;
    }

    public Long getLastHeartbeatTimeMs() {
        return lastHeartbeatTimeMs;
    }

    public void setLastHeartbeatTimeMs(Long lastHeartbeatTimeMs) {
        this.lastHeartbeatTimeMs = lastHeartbeatTimeMs;
    }

    public String getLastIp() {
        return lastIp;
    }

    public void setLastIp(String lastIp) {
        this.lastIp = lastIp;
    }
}

