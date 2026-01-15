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
        String alarmFile,
        String storeNum,
        String storeroomNum,
        Long warningTime,
        Long clearTime,
        Long archivedTime,
        Long handledTime,
        String handler,
        String remark
) {
}
