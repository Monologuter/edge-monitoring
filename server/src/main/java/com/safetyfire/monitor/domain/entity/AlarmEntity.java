package com.safetyfire.monitor.domain.entity;

/**
 * 告警事件实体。
 */
public class AlarmEntity {
    private Long id;
    private String companyCode;
    private String alarmType;
    private String alarmStatus;
    private String workflowStatus;
    private String riskLevel;
    private String deviceCode;
    private Long warningTime;
    private Long clearTime;
    private Long archivedTime;
    private String handler;
    private String remark;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getCompanyCode() {
        return companyCode;
    }

    public void setCompanyCode(String companyCode) {
        this.companyCode = companyCode;
    }

    public String getAlarmType() {
        return alarmType;
    }

    public void setAlarmType(String alarmType) {
        this.alarmType = alarmType;
    }

    public String getAlarmStatus() {
        return alarmStatus;
    }

    public void setAlarmStatus(String alarmStatus) {
        this.alarmStatus = alarmStatus;
    }

    public String getWorkflowStatus() {
        return workflowStatus;
    }

    public void setWorkflowStatus(String workflowStatus) {
        this.workflowStatus = workflowStatus;
    }

    public String getRiskLevel() {
        return riskLevel;
    }

    public void setRiskLevel(String riskLevel) {
        this.riskLevel = riskLevel;
    }

    public String getDeviceCode() {
        return deviceCode;
    }

    public void setDeviceCode(String deviceCode) {
        this.deviceCode = deviceCode;
    }

    public Long getWarningTime() {
        return warningTime;
    }

    public void setWarningTime(Long warningTime) {
        this.warningTime = warningTime;
    }

    public Long getClearTime() {
        return clearTime;
    }

    public void setClearTime(Long clearTime) {
        this.clearTime = clearTime;
    }

    public Long getArchivedTime() {
        return archivedTime;
    }

    public void setArchivedTime(Long archivedTime) {
        this.archivedTime = archivedTime;
    }

    public String getHandler() {
        return handler;
    }

    public void setHandler(String handler) {
        this.handler = handler;
    }

    public String getRemark() {
        return remark;
    }

    public void setRemark(String remark) {
        this.remark = remark;
    }
}
