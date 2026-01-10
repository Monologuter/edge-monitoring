package com.safetyfire.monitor.domain.entity;

/**
 * 仓库实体（FR-04）。
 */
public class StoreEntity {
    private Long id;
    private String companyCode;
    private String storeNum;
    private String storeName;
    private Double area;
    private String dangerLevel;
    private Double quotaDosage;
    private Integer quotaPeople;

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

    public String getStoreNum() {
        return storeNum;
    }

    public void setStoreNum(String storeNum) {
        this.storeNum = storeNum;
    }

    public String getStoreName() {
        return storeName;
    }

    public void setStoreName(String storeName) {
        this.storeName = storeName;
    }

    public Double getArea() {
        return area;
    }

    public void setArea(Double area) {
        this.area = area;
    }

    public String getDangerLevel() {
        return dangerLevel;
    }

    public void setDangerLevel(String dangerLevel) {
        this.dangerLevel = dangerLevel;
    }

    public Double getQuotaDosage() {
        return quotaDosage;
    }

    public void setQuotaDosage(Double quotaDosage) {
        this.quotaDosage = quotaDosage;
    }

    public Integer getQuotaPeople() {
        return quotaPeople;
    }

    public void setQuotaPeople(Integer quotaPeople) {
        this.quotaPeople = quotaPeople;
    }
}

