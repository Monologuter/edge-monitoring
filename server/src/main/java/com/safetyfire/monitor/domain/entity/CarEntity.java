package com.safetyfire.monitor.domain.entity;

/**
 * 车辆档案实体（FR-03）。
 */
public class CarEntity {
    private Long id;
    private String companyCode;
    private String licensePlateNumber;
    private String driverName;
    private String driverPhone;
    private java.time.LocalDate validStart;
    private java.time.LocalDate validEnd;
    private Long licenseFileId;
    private String carType;
    private java.time.LocalDateTime updatedAt;

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

    public String getDriverName() {
        return driverName;
    }

    public void setDriverName(String driverName) {
        this.driverName = driverName;
    }

    public String getDriverPhone() {
        return driverPhone;
    }

    public void setDriverPhone(String driverPhone) {
        this.driverPhone = driverPhone;
    }

    public java.time.LocalDate getValidStart() {
        return validStart;
    }

    public void setValidStart(java.time.LocalDate validStart) {
        this.validStart = validStart;
    }

    public java.time.LocalDate getValidEnd() {
        return validEnd;
    }

    public void setValidEnd(java.time.LocalDate validEnd) {
        this.validEnd = validEnd;
    }

    public Long getLicenseFileId() {
        return licenseFileId;
    }

    public void setLicenseFileId(Long licenseFileId) {
        this.licenseFileId = licenseFileId;
    }

    public String getCarType() {
        return carType;
    }

    public void setCarType(String carType) {
        this.carType = carType;
    }

    public java.time.LocalDateTime getUpdatedAt() {
        return updatedAt;
    }

    public void setUpdatedAt(java.time.LocalDateTime updatedAt) {
        this.updatedAt = updatedAt;
    }
}
