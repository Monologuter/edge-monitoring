package com.safetyfire.monitor.util;

import java.util.Base64;

/**
 * Base64 解码工具：兼容 data URI 前缀与空白字符。
 */
public final class AiBoxBase64 {
    private AiBoxBase64() {
    }

    public static byte[] decode(String base64) {
        if (base64 == null) return null;
        String s = base64.trim();
        if (s.isEmpty()) return null;
        int comma = s.indexOf(',');
        if (s.startsWith("data:") && comma >= 0) {
            s = s.substring(comma + 1);
        }
        // 兼容包含换行/空格的 base64
        s = s.replaceAll("\\s+", "");
        try {
            return Base64.getDecoder().decode(s);
        } catch (IllegalArgumentException e) {
            // 尝试 URL-safe
            return Base64.getUrlDecoder().decode(s);
        }
    }
}

