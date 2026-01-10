package com.safetyfire.monitor.domain.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;

import java.time.LocalDate;

/**
 * 企业更新入参。
 */
public record CompanyUpdateRequest(
        @NotNull Long id,
        @NotBlank @Size(max = 255) String companyName,
        @Size(max = 64) String businessLicense,
        Long businessLicenseFileId,
        LocalDate businessLicenseStart,
        LocalDate businessLicenseEnd,
        @Size(max = 512) String businessLicenseScope,
        @Size(max = 128) String businessLicenseIssuingAuthority,
        @Size(max = 255) String address,
        @Size(max = 255) String registerAddress,
        @Size(max = 32) String companyStatus,
        Double dosage,
        Double reservoirArea
) {
}

