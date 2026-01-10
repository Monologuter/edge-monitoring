package com.safetyfire.monitor.domain.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;

/**
 * 库房创建入参。
 */
public record StoreroomCreateRequest(
        @NotBlank @Size(max = 64) String storeNum,
        @NotBlank @Size(max = 64) String storeroomNum,
        @NotBlank @Size(max = 255) String storeroomName,
        Double area,
        @Size(max = 8) String dangerLevel,
        Double quotaDosage,
        Integer quotaPeople
) {
}

