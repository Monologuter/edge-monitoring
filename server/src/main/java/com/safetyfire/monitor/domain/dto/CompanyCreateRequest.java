package com.safetyfire.monitor.domain.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;

import java.time.LocalDate;

/**
 * 企业创建入参。
 */
public record CompanyCreateRequest(
        @NotBlank @Size(max = 64) String companyCode,
        @NotBlank @Size(max = 255) String companyName,
        @Size(max = 64) String creditCode,
        @Size(max = 64) String principalName,
        @Size(max = 64) String businessLicense,
        Long businessLicenseFileId,
        LocalDate businessLicenseStart,
        LocalDate businessLicenseEnd,
        @Size(max = 512) String businessLicenseScope,
        @Size(max = 128) String businessLicenseIssuingAuthority,
        @Size(max = 255) String address,
        @Size(max = 255) String registerAddress,
        @Size(max = 255) String storageAddress,
        @Size(max = 32) String companyStatus,
        Double dosage,
        Double reservoirArea,
        Double storeroomArea,
        Double longitude,
        Double latitude
) {
}
