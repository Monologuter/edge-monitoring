package com.safetyfire.monitor.domain.vo;

/**
 * 车辆出入记录视图对象。
 */
public record CarInoutRecordVO(
        Long id,
        String companyCode,
        String licensePlateNumber,
        String carType,
        String driverName,
        String inOutState,
        Long inOutTime,
        Long imageFileId
) {
}
