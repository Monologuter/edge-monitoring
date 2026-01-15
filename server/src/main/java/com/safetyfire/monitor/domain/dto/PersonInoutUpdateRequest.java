package com.safetyfire.monitor.domain.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;

/**
 * 人员出入记录手工编辑。
 */
public record PersonInoutUpdateRequest(
        @NotNull Long id,
        String companyCode,
        @NotBlank @Size(max = 64) String idcard,
        @NotBlank @Size(max = 64) String personName,
        @NotBlank @Size(max = 64) String personType,
        @NotBlank @Size(max = 8) String inOutState,
        @NotNull Long inOutTime,
        Long imageFileId
) {
}
