package com.safetyfire.monitor.domain.dto;

import jakarta.validation.constraints.*;

/**
 * 人员创建入参。
 */
public record PersonCreateRequest(
        @NotBlank @Size(max = 64) String companyCode,
        @NotBlank @Size(max = 64) String personName,
        @NotBlank @Size(max = 32) String idcard,
        @NotBlank @Size(max = 32) String personType,
        @NotNull @Min(0) @Max(1) Integer isCertified,
        @Size(max = 32) String phone,
        Long avatarFileId,
        Long certFileId,
        java.time.LocalDate certExpireDate,
        @Min(0) @Max(1) Integer smsNotify
) {
}
