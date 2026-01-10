package com.safetyfire.monitor.security;

/**
 * 线程上下文中的登录用户信息（便于业务层使用）。
 */
public final class AuthUserHolder {
    private static final ThreadLocal<AuthUser> TL = new ThreadLocal<>();

    private AuthUserHolder() {
    }

    public static void set(AuthUser user) {
        TL.set(user);
    }

    public static AuthUser get() {
        return TL.get();
    }

    public static void clear() {
        TL.remove();
    }
}

