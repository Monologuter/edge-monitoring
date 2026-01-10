package com.safetyfire.monitor.domain.entity;

/**
 * 硬件上报日志实体：用于后台查看“硬件传递过来的原始数据”。
 */
public class HardwareIngestLogEntity {
    private Long id;
    private String ingestChannel;
    private String topic;
    private String messageType;
    private String apiKey;
    private String companyCode;
    private String payload;
    private Integer parsedOk;
    private String errorMessage;
    private Long receiveTimeMs;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getIngestChannel() {
        return ingestChannel;
    }

    public void setIngestChannel(String ingestChannel) {
        this.ingestChannel = ingestChannel;
    }

    public String getTopic() {
        return topic;
    }

    public void setTopic(String topic) {
        this.topic = topic;
    }

    public String getMessageType() {
        return messageType;
    }

    public void setMessageType(String messageType) {
        this.messageType = messageType;
    }

    public String getApiKey() {
        return apiKey;
    }

    public void setApiKey(String apiKey) {
        this.apiKey = apiKey;
    }

    public String getCompanyCode() {
        return companyCode;
    }

    public void setCompanyCode(String companyCode) {
        this.companyCode = companyCode;
    }

    public String getPayload() {
        return payload;
    }

    public void setPayload(String payload) {
        this.payload = payload;
    }

    public Integer getParsedOk() {
        return parsedOk;
    }

    public void setParsedOk(Integer parsedOk) {
        this.parsedOk = parsedOk;
    }

    public String getErrorMessage() {
        return errorMessage;
    }

    public void setErrorMessage(String errorMessage) {
        this.errorMessage = errorMessage;
    }

    public Long getReceiveTimeMs() {
        return receiveTimeMs;
    }

    public void setReceiveTimeMs(Long receiveTimeMs) {
        this.receiveTimeMs = receiveTimeMs;
    }
}

