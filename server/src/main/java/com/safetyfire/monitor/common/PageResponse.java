package com.safetyfire.monitor.common;

import java.util.List;

/**
 * 分页响应体（稳定排序由 SQL 保证）。
 */
public record PageResponse<T>(List<T> list, int page, int pageSize, long total) {
}

