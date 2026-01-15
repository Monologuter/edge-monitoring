package com.safetyfire.monitor.domain.vo;

/**
 * 人员视图对象（身份证脱敏）。
 */
public record PersonVO(
        Long id,
        String companyCode,
        String personName,
        String idcardMasked,
        String personType,
        Integer isCertified,
        String phone,
        Long avatarFileId,
        Long certFileId,
        java.time.LocalDate certExpireDate,
        Integer smsNotify,
        Long dataSyncTime
) {
}
