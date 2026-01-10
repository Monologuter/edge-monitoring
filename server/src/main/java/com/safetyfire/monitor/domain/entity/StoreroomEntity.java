package com.safetyfire.monitor.domain.entity;

/**
 * 库房实体（FR-04）。
 */
public class StoreroomEntity {
    private Long id;
    private String storeNum;
    private String storeroomNum;
    private String storeroomName;
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

    public String getStoreNum() {
        return storeNum;
    }

    public void setStoreNum(String storeNum) {
        this.storeNum = storeNum;
    }

    public String getStoreroomNum() {
        return storeroomNum;
    }

    public void setStoreroomNum(String storeroomNum) {
        this.storeroomNum = storeroomNum;
    }

    public String getStoreroomName() {
        return storeroomName;
    }

    public void setStoreroomName(String storeroomName) {
        this.storeroomName = storeroomName;
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

