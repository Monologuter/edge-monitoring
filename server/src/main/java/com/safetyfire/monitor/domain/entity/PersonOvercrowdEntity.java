package com.safetyfire.monitor.domain.entity;

/**
 * 人员超员记录实体。
 */
public class PersonOvercrowdEntity {
    private Long id;
    private String companyCode;
    private String areaCode;
    private String areaName;
    private Integer currentCount;
    private Integer maxCount;
    private Long warningTime;

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

    public String getAreaCode() {
        return areaCode;
    }

    public void setAreaCode(String areaCode) {
        this.areaCode = areaCode;
    }

    public String getAreaName() {
        return areaName;
    }

    public void setAreaName(String areaName) {
        this.areaName = areaName;
    }

    public Integer getCurrentCount() {
        return currentCount;
    }

    public void setCurrentCount(Integer currentCount) {
        this.currentCount = currentCount;
    }

    public Integer getMaxCount() {
        return maxCount;
    }

    public void setMaxCount(Integer maxCount) {
        this.maxCount = maxCount;
    }

    public Long getWarningTime() {
        return warningTime;
    }

    public void setWarningTime(Long warningTime) {
        this.warningTime = warningTime;
    }
}
