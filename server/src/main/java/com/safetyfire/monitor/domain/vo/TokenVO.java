package com.safetyfire.monitor.domain.vo;

/**
 * 登录/刷新返回的令牌对象。
 */
public record TokenVO(String accessToken, String refreshToken, long expiresInSeconds) {
}

