package com.safetyfire.monitor.domain.dto;

import jakarta.validation.constraints.*;

/**
 * 人员更新入参。
 */
public record PersonUpdateRequest(
        @NotNull Long id,
        @NotBlank @Size(max = 64) String companyCode,
        @NotBlank @Size(max = 64) String personName,
        @NotBlank @Size(max = 32) String personType,
        @NotNull @Min(0) @Max(1) Integer isCertified,
        @Size(max = 32) String phone,
        Long avatarFileId,
        Long certFileId,
        java.time.LocalDate certExpireDate,
        @Min(0) @Max(1) Integer smsNotify
) {
}
