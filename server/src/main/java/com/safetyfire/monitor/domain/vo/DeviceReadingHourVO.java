package com.safetyfire.monitor.domain.vo;

/**
 * 设备小时聚合数据视图对象。
 */
public record DeviceReadingHourVO(String deviceCode, Long hourStart, Double avgValue, Double minValue, Double maxValue, Integer samples) {
}

