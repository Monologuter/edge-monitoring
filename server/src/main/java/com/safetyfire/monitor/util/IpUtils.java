package com.safetyfire.monitor.util;

import jakarta.servlet.http.HttpServletRequest;

/**
 * IP 工具：优先读取反向代理常见头。
 */
public final class IpUtils {
    private IpUtils() {
    }

    public static String getClientIp(HttpServletRequest request) {
        String xff = request.getHeader("X-Forwarded-For");
        if (xff != null && !xff.isBlank()) {
            // 取第一个 IP
            int idx = xff.indexOf(',');
            return (idx > 0 ? xff.substring(0, idx) : xff).trim();
        }
        String realIp = request.getHeader("X-Real-IP");
        if (realIp != null && !realIp.isBlank()) {
            return realIp.trim();
        }
        String remote = request.getRemoteAddr();
        return remote == null ? "" : remote;
    }
}

