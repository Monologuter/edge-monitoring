package com.safetyfire.monitor.domain.vo;

import java.util.List;

/**
 * 当前用户信息。
 */
public record UserVO(Long id, String username, String displayName, List<String> roles, List<String> permissions,
                     List<MenuVO> menus, List<String> companyCodes) {
}
