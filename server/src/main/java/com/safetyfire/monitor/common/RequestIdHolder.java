package com.safetyfire.monitor.common;

/**
 * 请求链路 ID（用于日志与响应体）。
 */
public final class RequestIdHolder {
    private static final ThreadLocal<String> TL = new ThreadLocal<>();

    private RequestIdHolder() {
    }

    public static void set(String requestId) {
        TL.set(requestId);
    }

    public static String get() {
        return TL.get();
    }

    public static void clear() {
        TL.remove();
    }
}

