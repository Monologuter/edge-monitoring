package com.safetyfire.monitor.domain.dto;

import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;

/**
 * 通用分页查询参数。
 */
public record PageQuery(
        @Min(1) int page,
        @Min(1) @Max(200) int pageSize
) {
    public int offset() {
        return (page - 1) * pageSize;
    }
}

