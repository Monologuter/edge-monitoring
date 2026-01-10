package com.safetyfire.monitor.domain.vo;

import java.util.List;

/**
 * 管理后台：用户信息。
 */
public record AdminUserVO(
        Long id,
        String username,
        String displayName,
        Integer enabled,
        List<String> roleKeys,
        List<String> companyCodes
) {
}

