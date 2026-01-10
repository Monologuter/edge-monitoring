package com.safetyfire.monitor.domain.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;

import java.util.List;

/**
 * 管理后台：创建用户入参。
 */
public record AdminUserCreateRequest(
        @NotBlank @Size(max = 64) String username,
        @NotBlank @Size(max = 64) String displayName,
        @NotBlank @Size(min = 8, max = 64) String password,
        @NotNull Integer enabled,
        List<String> roleKeys,
        List<String> companyCodes
) {
}

