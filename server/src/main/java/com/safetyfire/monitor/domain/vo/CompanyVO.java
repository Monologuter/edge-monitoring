package com.safetyfire.monitor.domain.vo;

import java.time.LocalDate;

/**
 * 企业视图对象。
 */
public record CompanyVO(
        Long id,
        String companyCode,
        String companyName,
        String businessLicense,
        Long businessLicenseFileId,
        LocalDate businessLicenseStart,
        LocalDate businessLicenseEnd,
        String businessLicenseScope,
        String businessLicenseIssuingAuthority,
        String address,
        String registerAddress,
        String companyStatus,
        Double dosage,
        Double reservoirArea
) {
}

