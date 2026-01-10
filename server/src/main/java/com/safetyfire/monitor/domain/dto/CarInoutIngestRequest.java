package com.safetyfire.monitor.domain.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;

/**
 * 车辆出入记录接入（硬件上报）。
 */
public record CarInoutIngestRequest(
        @NotBlank @Size(max = 32) String licensePlateNumber,
        @Size(max = 32) String carType,
        @NotBlank @Size(max = 8) String inOutState,
        @NotNull Long inOutTime,
        Long imageFileId
) {
}

