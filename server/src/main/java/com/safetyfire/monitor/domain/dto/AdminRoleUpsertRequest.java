package com.safetyfire.monitor.domain.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;

import java.util.List;

/**
 * 管理后台：角色创建/更新入参。
 */
public record AdminRoleUpsertRequest(
        Long id,
        @NotBlank @Size(max = 64) String roleKey,
        @NotBlank @Size(max = 64) String roleName,
        @NotNull List<String> permissionKeys,
        @NotNull List<String> menuKeys
) {
}

