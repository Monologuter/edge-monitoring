package com.safetyfire.monitor.config;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

/**
 * 认证相关配置。
 */
@Component
@ConfigurationProperties(prefix = "app.auth")
public class AuthProperties {
    /**
     * 是否启用登录验证码。
     */
    private boolean captchaEnabled;

    /**
     * 验证码长度。
     */
    private int captchaLength;

    /**
     * 验证码有效期（秒）。
     */
    private int captchaTtlSeconds;

    public boolean isCaptchaEnabled() {
        return captchaEnabled;
    }

    public void setCaptchaEnabled(boolean captchaEnabled) {
        this.captchaEnabled = captchaEnabled;
    }

    public int getCaptchaLength() {
        return captchaLength;
    }

    public void setCaptchaLength(int captchaLength) {
        this.captchaLength = captchaLength;
    }

    public int getCaptchaTtlSeconds() {
        return captchaTtlSeconds;
    }

    public void setCaptchaTtlSeconds(int captchaTtlSeconds) {
        this.captchaTtlSeconds = captchaTtlSeconds;
    }
}
