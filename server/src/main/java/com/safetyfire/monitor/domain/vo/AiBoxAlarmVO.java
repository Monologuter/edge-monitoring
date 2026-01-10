package com.safetyfire.monitor.domain.vo;

/**
 * AI盒子告警记录VO。
 */
public record AiBoxAlarmVO(
        Long id,
        String companyCode,
        String alarmType,
        String alarmStatus,
        Long warningTime,
        String cameraCode,
        String areaCode,
        Double height,
        Double weight,
        String description,
        Long imageFileId
) {
}
