package com.safetyfire.monitor.domain.entity;

/**
 * 人员档案实体（FR-02）。
 */
public class PersonEntity {
    private Long id;
    private String companyCode;
    private String personName;
    private String idcard;
    private String personType;
    private Integer isCertified;
    private String phone;
    private Long avatarFileId;
    private Long certFileId;
    private java.time.LocalDate certExpireDate;
    private Integer smsNotify;
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

    public String getPersonName() {
        return personName;
    }

    public void setPersonName(String personName) {
        this.personName = personName;
    }

    public String getIdcard() {
        return idcard;
    }

    public void setIdcard(String idcard) {
        this.idcard = idcard;
    }

    public String getPersonType() {
        return personType;
    }

    public void setPersonType(String personType) {
        this.personType = personType;
    }

    public Integer getIsCertified() {
        return isCertified;
    }

    public void setIsCertified(Integer isCertified) {
        this.isCertified = isCertified;
    }

    public String getPhone() {
        return phone;
    }

    public void setPhone(String phone) {
        this.phone = phone;
    }

    public Long getAvatarFileId() {
        return avatarFileId;
    }

    public void setAvatarFileId(Long avatarFileId) {
        this.avatarFileId = avatarFileId;
    }

    public Long getCertFileId() {
        return certFileId;
    }

    public void setCertFileId(Long certFileId) {
        this.certFileId = certFileId;
    }

    public java.time.LocalDate getCertExpireDate() {
        return certExpireDate;
    }

    public void setCertExpireDate(java.time.LocalDate certExpireDate) {
        this.certExpireDate = certExpireDate;
    }

    public Integer getSmsNotify() {
        return smsNotify;
    }

    public void setSmsNotify(Integer smsNotify) {
        this.smsNotify = smsNotify;
    }

    public java.time.LocalDateTime getUpdatedAt() {
        return updatedAt;
    }

    public void setUpdatedAt(java.time.LocalDateTime updatedAt) {
        this.updatedAt = updatedAt;
    }
}
