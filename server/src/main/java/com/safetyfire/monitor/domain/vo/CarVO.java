package com.safetyfire.monitor.domain.vo;

/**
 * 车辆视图对象。
 */
public record CarVO(
        Long id,
        String companyCode,
        String licensePlateNumber,
        String carType
) {
}

