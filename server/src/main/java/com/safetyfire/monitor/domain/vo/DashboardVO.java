package com.safetyfire.monitor.domain.vo;

import java.util.List;

/**
 * 首页概览数据。
 */
public record DashboardVO(
        long activeAlarms,
        long todayAlarms,
        long onlineDevices,
        long totalDevices,
        List<Long> alarmTrend24h
) {
}

