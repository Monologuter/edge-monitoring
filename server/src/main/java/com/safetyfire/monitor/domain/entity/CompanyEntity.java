package com.safetyfire.monitor.domain.entity;

import java.time.LocalDate;

/**
 * 企业信息实体（FR-01）。
 */
public class CompanyEntity {
    private Long id;
    private String companyCode;
    private String companyName;
    private String creditCode;
    private String principalName;
    private String businessLicense;
    private Long businessLicenseFileId;
    private LocalDate businessLicenseStart;
    private LocalDate businessLicenseEnd;
    private String businessLicenseScope;
    private String businessLicenseIssuingAuthority;
    private String address;
    private String registerAddress;
    private String storageAddress;
    private String companyStatus;
    private Double dosage;
    private Double reservoirArea;
    private Double storeroomArea;
    private Double longitude;
    private Double latitude;

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

    public String getCompanyName() {
        return companyName;
    }

    public void setCompanyName(String companyName) {
        this.companyName = companyName;
    }

    public String getCreditCode() {
        return creditCode;
    }

    public void setCreditCode(String creditCode) {
        this.creditCode = creditCode;
    }

    public String getPrincipalName() {
        return principalName;
    }

    public void setPrincipalName(String principalName) {
        this.principalName = principalName;
    }

    public String getBusinessLicense() {
        return businessLicense;
    }

    public void setBusinessLicense(String businessLicense) {
        this.businessLicense = businessLicense;
    }

    public Long getBusinessLicenseFileId() {
        return businessLicenseFileId;
    }

    public void setBusinessLicenseFileId(Long businessLicenseFileId) {
        this.businessLicenseFileId = businessLicenseFileId;
    }

    public LocalDate getBusinessLicenseStart() {
        return businessLicenseStart;
    }

    public void setBusinessLicenseStart(LocalDate businessLicenseStart) {
        this.businessLicenseStart = businessLicenseStart;
    }

    public LocalDate getBusinessLicenseEnd() {
        return businessLicenseEnd;
    }

    public void setBusinessLicenseEnd(LocalDate businessLicenseEnd) {
        this.businessLicenseEnd = businessLicenseEnd;
    }

    public String getBusinessLicenseScope() {
        return businessLicenseScope;
    }

    public void setBusinessLicenseScope(String businessLicenseScope) {
        this.businessLicenseScope = businessLicenseScope;
    }

    public String getBusinessLicenseIssuingAuthority() {
        return businessLicenseIssuingAuthority;
    }

    public void setBusinessLicenseIssuingAuthority(String businessLicenseIssuingAuthority) {
        this.businessLicenseIssuingAuthority = businessLicenseIssuingAuthority;
    }

    public String getAddress() {
        return address;
    }

    public void setAddress(String address) {
        this.address = address;
    }

    public String getRegisterAddress() {
        return registerAddress;
    }

    public void setRegisterAddress(String registerAddress) {
        this.registerAddress = registerAddress;
    }

    public String getStorageAddress() {
        return storageAddress;
    }

    public void setStorageAddress(String storageAddress) {
        this.storageAddress = storageAddress;
    }

    public String getCompanyStatus() {
        return companyStatus;
    }

    public void setCompanyStatus(String companyStatus) {
        this.companyStatus = companyStatus;
    }

    public Double getDosage() {
        return dosage;
    }

    public void setDosage(Double dosage) {
        this.dosage = dosage;
    }

    public Double getReservoirArea() {
        return reservoirArea;
    }

    public void setReservoirArea(Double reservoirArea) {
        this.reservoirArea = reservoirArea;
    }

    public Double getStoreroomArea() {
        return storeroomArea;
    }

    public void setStoreroomArea(Double storeroomArea) {
        this.storeroomArea = storeroomArea;
    }

    public Double getLongitude() {
        return longitude;
    }

    public void setLongitude(Double longitude) {
        this.longitude = longitude;
    }

    public Double getLatitude() {
        return latitude;
    }

    public void setLatitude(Double latitude) {
        this.latitude = latitude;
    }
}
