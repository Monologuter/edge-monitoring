package com.safetyfire.monitor.domain.vo;

/**
 * 告警视图对象。
 */
public record AlarmVO(
        Long id,
        String companyCode,
        String alarmType,
        String alarmStatus,
        String workflowStatus,
        String riskLevel,
        String deviceCode,
        Long warningTime,
        Long clearTime,
        Long archivedTime,
        String handler,
        String remark
) {
}
