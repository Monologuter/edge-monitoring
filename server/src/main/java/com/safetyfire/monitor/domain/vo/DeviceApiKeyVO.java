package com.safetyfire.monitor.domain.vo;

/**
 * 设备API密钥视图对象。
 */
public record DeviceApiKeyVO(
        Long id,
        String apiKey,
        String apiSecret,
        Integer enabled,
        String allowedIps,
        Integer rateLimitPerMinute
) {
}
