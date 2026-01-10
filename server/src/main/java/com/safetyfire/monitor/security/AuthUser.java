package com.safetyfire.monitor.security;

import java.util.List;

/**
 * 当前登录用户信息（用于 JWT / 鉴权上下文）。
 */
public record AuthUser(Long userId, String username, String displayName, List<String> roles, List<String> permissions) {
}
