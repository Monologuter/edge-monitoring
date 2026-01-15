package com.safetyfire.monitor.domain.vo;

import java.util.List;

/**
 * 导入结果。
 */
public record ImportResultVO(
        int successCount,
        int failCount,
        List<String> errors
) {
}
