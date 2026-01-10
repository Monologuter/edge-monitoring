package com.safetyfire.monitor.domain.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;

/**
 * 摄像头创建入参。
 */
public record CameraCreateRequest(
        @Size(max = 64) String companyCode,
        @NotBlank @Size(max = 64) String cameraCode,
        @NotBlank @Size(max = 255) String cameraName,
        @NotBlank @Size(max = 512) String streamUrl,
        @Size(max = 255) String locationName,
        @Size(max = 64) String storeNum,
        @Size(max = 64) String storeroomNum,
        @NotNull Integer enabled
) {
}

