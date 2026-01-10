package com.safetyfire.monitor.common;

/**
 * 错误码规范：0 成功；1xxx 通用；2xxx 鉴权；3xxx 业务域。
 */
public enum ErrorCode {
    OK(0),
    PARAM_INVALID(1001),
    NOT_FOUND(1002),
    STATE_CONFLICT(1003),
    RATE_LIMITED(1100),
    SYSTEM_ERROR(1500),
    UNAUTHORIZED(2001),
    TOKEN_EXPIRED(2002),
    FORBIDDEN(2003),
    LOGIN_FAILED(2004),
    DEVICE_AUTH_FAILED(2101);

    private final int code;

    ErrorCode(int code) {
        this.code = code;
    }

    public int code() {
        return code;
    }
}
