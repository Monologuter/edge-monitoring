package com.safetyfire.monitor.util;

/**
 * 脱敏工具：避免日志与审计中泄露敏感信息。
 */
public final class MaskUtils {
    private MaskUtils() {
    }

    public static String maskIdCard(String idcard) {
        if (idcard == null || idcard.isBlank()) return idcard;
        String s = idcard.trim();
        if (s.length() <= 8) return "****";
        return s.substring(0, 4) + "********" + s.substring(s.length() - 4);
    }

    public static String maskToken(String token) {
        if (token == null || token.isBlank()) return token;
        String s = token.trim();
        if (s.length() <= 12) return "****";
        return s.substring(0, 6) + "..." + s.substring(s.length() - 6);
    }

    public static String maskPassword(String any) {
        if (any == null) return null;
        return "******";
    }
}

