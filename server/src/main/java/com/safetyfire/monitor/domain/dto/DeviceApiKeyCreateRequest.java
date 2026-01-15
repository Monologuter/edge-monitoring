package com.safetyfire.monitor.domain.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;

/**
 * 设备API密钥创建请求。
 */
public record DeviceApiKeyCreateRequest(
        @NotBlank String apiKey,
        @NotBlank String apiSecret,
        @NotNull Integer enabled,
        String allowedIps,
        Integer rateLimitPerMinute
) {
}
