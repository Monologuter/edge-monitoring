package com.safetyfire.monitor.domain.vo;

import java.util.List;

/**
 * 管理后台：角色信息。
 */
public record AdminRoleVO(
        Long id,
        String roleKey,
        String roleName,
        List<String> permissionKeys,
        List<String> menuKeys
) {
}

