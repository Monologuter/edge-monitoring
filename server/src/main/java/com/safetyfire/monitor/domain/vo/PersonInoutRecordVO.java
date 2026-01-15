package com.safetyfire.monitor.domain.vo;

/**
 * 人员出入记录视图对象。
 */
public record PersonInoutRecordVO(
        Long id,
        String companyCode,
        String idcard,
        String idcardMasked,
        String personName,
        String personType,
        String inOutState,
        Long inOutTime,
        Long imageFileId
) {
}
