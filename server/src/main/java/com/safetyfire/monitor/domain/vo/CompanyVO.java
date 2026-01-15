package com.safetyfire.monitor.domain.vo;

import java.time.LocalDate;

/**
 * 企业视图对象。
 */
public record CompanyVO(
        Long id,
        String companyCode,
        String companyName,
        String creditCode,
        String principalName,
        String businessLicense,
        Long businessLicenseFileId,
        LocalDate businessLicenseStart,
        LocalDate businessLicenseEnd,
        String businessLicenseScope,
        String businessLicenseIssuingAuthority,
        String address,
        String registerAddress,
        String storageAddress,
        String companyStatus,
        Double dosage,
        Double reservoirArea,
        Double storeroomArea,
        Double longitude,
        Double latitude
) {
}
