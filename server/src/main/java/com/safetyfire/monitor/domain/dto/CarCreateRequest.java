package com.safetyfire.monitor.domain.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;

/**
 * 车辆创建入参。
 */
public record CarCreateRequest(
        @NotBlank @Size(max = 64) String companyCode,
        @NotBlank @Size(max = 32) String licensePlateNumber,
        @Size(max = 64) String driverName,
        @Size(max = 32) String driverPhone,
        java.time.LocalDate validStart,
        java.time.LocalDate validEnd,
        Long licenseFileId,
        @Size(max = 32) String carType
) {
}
