package com.safetyfire.monitor.domain.vo;

import java.util.List;

/**
 * 首页概览数据。
 */
public record DashboardVO(
        String companyName,
        String businessLicense,
        Double temperatureValue,
        Double humidityValue,
        Double levelValue,
        long onsitePeople,
        long onsiteCars,
        long storeroomCount,
        long todayInPeople,
        long todayOutPeople,
        long todayInCars,
        long todayOutCars,
        long onlineDevices,
        long totalDevices,
        double onlineRate,
        long activeAlarms,
        long todayAlarms,
        List<Long> alarmTrend24h
) {
}
