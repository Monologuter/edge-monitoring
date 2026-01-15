package com.safetyfire.monitor.domain.dto;

import jakarta.validation.constraints.NotNull;

/**
 * 设备API密钥更新请求。
 */
public record DeviceApiKeyUpdateRequest(
        @NotNull Long id,
        String apiSecret,
        Integer enabled,
        String allowedIps,
        Integer rateLimitPerMinute
) {
}
