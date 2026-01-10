package com.safetyfire.monitor.domain.dto;

import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;

/**
 * 管理后台：重置用户密码入参。
 */
public record AdminUserResetPasswordRequest(
        @NotNull Long userId,
        @NotBlank @Size(min = 8, max = 64) String newPassword
) {
}

