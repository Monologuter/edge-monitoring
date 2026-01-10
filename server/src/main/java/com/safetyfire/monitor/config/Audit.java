package com.safetyfire.monitor.config;

import java.lang.annotation.*;

/**
 * 审计点标记：用于记录关键操作。
 */
@Target({ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
@Documented
public @interface Audit {
    String action();
}

