package com.safetyfire.monitor.domain.vo;

/**
 * 设备视图对象。
 */
public record DeviceVO(
        Long id,
        String companyCode,
        String deviceCode,
        String deviceName,
        Integer deviceType,
        String unit,
        Double lowerLimit,
        Double upperLimit,
        String locationName,
        String storeNum,
        String storeroomNum,
        String ipAddress,
        String accessUsername,
        String accessPassword,
        Integer onlineStatus
) {
}
