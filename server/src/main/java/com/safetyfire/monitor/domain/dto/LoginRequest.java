package com.safetyfire.monitor.domain.dto;

import jakarta.validation.constraints.NotBlank;

/**
 * 登录入参。
 */
public record LoginRequest(
        @NotBlank(message = "用户名不能为空") String username,
        @NotBlank(message = "密码不能为空") String password,
        String captchaId,
        String captchaCode
) {
}
