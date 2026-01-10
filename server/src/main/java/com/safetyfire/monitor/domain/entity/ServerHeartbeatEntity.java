package com.safetyfire.monitor.domain.entity;

/**
 * 服务器心跳实体。
 */
public class ServerHeartbeatEntity {
    private Long id;
    private String ip;
    private Long heartbeatTime;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getIp() {
        return ip;
    }

    public void setIp(String ip) {
        this.ip = ip;
    }

    public Long getHeartbeatTime() {
        return heartbeatTime;
    }

    public void setHeartbeatTime(Long heartbeatTime) {
        this.heartbeatTime = heartbeatTime;
    }
}

