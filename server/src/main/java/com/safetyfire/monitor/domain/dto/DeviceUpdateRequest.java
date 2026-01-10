package com.safetyfire.monitor.domain.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;

/**
 * 设备更新入参（FR-08）。
 */
public record DeviceUpdateRequest(
        @NotNull Long id,
        @Size(max = 64) String companyCode,
        @NotBlank @Size(max = 64) String deviceCode,
        @NotBlank @Size(max = 128) String deviceName,
        @NotNull Integer deviceType,
        @Size(max = 16) String unit,
        Double lowerLimit,
        Double upperLimit,
        @Size(max = 128) String locationName,
        @Size(max = 64) String storeNum,
        @Size(max = 64) String storeroomNum,
        @NotNull Integer onlineStatus
) {
}

