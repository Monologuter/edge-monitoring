package com.safetyfire.monitor.domain.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;

/**
 * 仓库创建入参。
 */
public record StoreCreateRequest(
        @NotBlank @Size(max = 64) String companyCode,
        @NotBlank @Size(max = 64) String storeNum,
        @NotBlank @Size(max = 255) String storeName,
        Double area,
        @Size(max = 8) String dangerLevel,
        Double quotaDosage,
        Integer quotaPeople
) {
}

