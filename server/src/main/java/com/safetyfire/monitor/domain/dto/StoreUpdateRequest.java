package com.safetyfire.monitor.domain.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;

/**
 * 仓库更新入参。
 */
public record StoreUpdateRequest(
        @NotNull Long id,
        @NotBlank @Size(max = 64) String companyCode,
        @NotBlank @Size(max = 64) String storeNum,
        @NotBlank @Size(max = 255) String storeName,
        Double area,
        @Size(max = 8) String dangerLevel,
        Double quotaDosage,
        Integer quotaPeople
) {
}

