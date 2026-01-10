package com.safetyfire.monitor.common;

import java.time.Instant;

/**
 * 统一响应体。
 */
public record ApiResponse<T>(
        int code,
        String message,
        T data,
        String requestId,
        long timestamp
) {
    public static <T> ApiResponse<T> ok(T data) {
        return new ApiResponse<>(0, "ok", data, RequestIdHolder.get(), Instant.now().toEpochMilli());
    }

    public static <T> ApiResponse<T> fail(ErrorCode errorCode, String message) {
        return new ApiResponse<>(errorCode.code(), message, null, RequestIdHolder.get(), Instant.now().toEpochMilli());
    }
}

