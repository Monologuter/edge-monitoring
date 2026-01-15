package com.safetyfire.monitor.config;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

/**
 * 服务器心跳在线判定配置。
 */
@Component
@ConfigurationProperties(prefix = "app.server-heartbeat")
public class ServerHeartbeatProperties {
    /**
     * 在线判定阈值（毫秒）：超过该时间未心跳则离线。
     */
    private long onlineThresholdMs;

    public long getOnlineThresholdMs() {
        return onlineThresholdMs;
    }

    public void setOnlineThresholdMs(long onlineThresholdMs) {
        this.onlineThresholdMs = onlineThresholdMs;
    }
}
