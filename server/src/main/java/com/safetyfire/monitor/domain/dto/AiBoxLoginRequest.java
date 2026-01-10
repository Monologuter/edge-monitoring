package com.safetyfire.monitor.domain.dto;

import jakarta.validation.constraints.NotBlank;

/**
 * AI盒子登录请求。
 */
public record AiBoxLoginRequest(@NotBlank(message = "deviceSerial不能为空") String deviceSerial) {
}

