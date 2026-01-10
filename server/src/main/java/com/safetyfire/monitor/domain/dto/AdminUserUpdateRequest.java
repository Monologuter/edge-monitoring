package com.safetyfire.monitor.domain.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;

import java.util.List;

/**
 * 管理后台：更新用户入参。
 */
public record AdminUserUpdateRequest(
        @NotNull Long id,
        @NotBlank @Size(max = 64) String displayName,
        @NotNull Integer enabled,
        List<String> roleKeys,
        List<String> companyCodes
) {
}

