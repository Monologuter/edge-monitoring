package com.safetyfire.monitor.domain.entity;

/**
 * 人员出入记录实体（FR-02）。
 */
public class PersonInoutRecordEntity {
    private Long id;
    private String companyCode;
    private String idcard;
    private String personName;
    private String personType;
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

    public String getIdcard() {
        return idcard;
    }

    public void setIdcard(String idcard) {
        this.idcard = idcard;
    }

    public String getPersonName() {
        return personName;
    }

    public void setPersonName(String personName) {
        this.personName = personName;
    }

    public String getPersonType() {
        return personType;
    }

    public void setPersonType(String personType) {
        this.personType = personType;
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
