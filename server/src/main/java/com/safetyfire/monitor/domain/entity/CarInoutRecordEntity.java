package com.safetyfire.monitor.domain.entity;

/**
 * 车辆出入记录实体（FR-03）。
 */
public class CarInoutRecordEntity {
    private Long id;
    private String companyCode;
    private String licensePlateNumber;
    private String carType;
    private String driverName;
    private String inOutState;
    private Long inOutTime;
    private Long imageFileId;

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

    public String getLicensePlateNumber() {
        return licensePlateNumber;
    }

    public void setLicensePlateNumber(String licensePlateNumber) {
        this.licensePlateNumber = licensePlateNumber;
    }

    public String getCarType() {
        return carType;
    }

    public void setCarType(String carType) {
        this.carType = carType;
    }

    public String getDriverName() {
        return driverName;
    }

    public void setDriverName(String driverName) {
        this.driverName = driverName;
    }

    public String getInOutState() {
        return inOutState;
    }

    public void setInOutState(String inOutState) {
        this.inOutState = inOutState;
    }

    public Long getInOutTime() {
        return inOutTime;
    }

    public void setInOutTime(Long inOutTime) {
        this.inOutTime = inOutTime;
    }

    public Long getImageFileId() {
        return imageFileId;
    }

    public void setImageFileId(Long imageFileId) {
        this.imageFileId = imageFileId;
    }
}
