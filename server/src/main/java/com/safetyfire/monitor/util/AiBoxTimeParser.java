package com.safetyfire.monitor.util;

import java.time.*;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;

/**
 * AI盒子时间字段解析：兼容 yyyy-MM-dd HH:mm:ss 与毫秒时间戳。
 */
public final class AiBoxTimeParser {
    private static final DateTimeFormatter DT = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
    private static final ZoneId ZONE = ZoneId.of("Asia/Shanghai");

    private AiBoxTimeParser() {
    }

    public static Long tryParseToEpochMs(Object value) {
        if (value == null) return null;
        if (value instanceof Number n) {
            long v = n.longValue();
            // 兼容秒级时间戳
            if (v > 0 && v < 10_000_000_000L) return v * 1000L;
            return v;
        }
        String s = String.valueOf(value).trim();
        if (s.isEmpty()) return null;
        try {
            long v = Long.parseLong(s);
            if (v > 0 && v < 10_000_000_000L) return v * 1000L;
            return v;
        } catch (NumberFormatException ignore) {
        }
        try {
            LocalDateTime ldt = LocalDateTime.parse(s, DT);
            return ldt.atZone(ZONE).toInstant().toEpochMilli();
        } catch (DateTimeParseException ignore) {
        }
        return null;
    }
}

