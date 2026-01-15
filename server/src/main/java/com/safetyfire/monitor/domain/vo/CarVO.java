package com.safetyfire.monitor.domain.vo;

/**
 * 车辆视图对象。
 */
public record CarVO(
        Long id,
        String companyCode,
        String licensePlateNumber,
        String driverName,
        String driverPhone,
        java.time.LocalDate validStart,
        java.time.LocalDate validEnd,
        Long licenseFileId,
        String carType,
        Long dataSyncTime
) {
}
