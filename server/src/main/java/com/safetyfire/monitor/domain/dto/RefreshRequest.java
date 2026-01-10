package com.safetyfire.monitor.domain.dto;

import jakarta.validation.constraints.NotBlank;

/**
 * 刷新令牌入参。
 */
public record RefreshRequest(@NotBlank String refreshToken) {
}

