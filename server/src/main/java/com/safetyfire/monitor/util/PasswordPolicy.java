package com.safetyfire.monitor.util;

/**
 * 密码策略（简化版）：至少包含大小写字母与数字/符号。
 */
public final class PasswordPolicy {
    private PasswordPolicy() {
    }

    public static boolean isStrong(String pwd) {
        if (pwd == null) return false;
        boolean hasUpper = false, hasLower = false, hasDigit = false, hasOther = false;
        for (char c : pwd.toCharArray()) {
            if (Character.isUpperCase(c)) hasUpper = true;
            else if (Character.isLowerCase(c)) hasLower = true;
            else if (Character.isDigit(c)) hasDigit = true;
            else hasOther = true;
        }
        return (hasUpper && hasLower && (hasDigit || hasOther));
    }
}

