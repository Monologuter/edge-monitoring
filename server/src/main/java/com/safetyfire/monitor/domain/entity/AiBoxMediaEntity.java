package com.safetyfire.monitor.domain.entity;

/**
 * AI盒子媒体上传记录（图片/录像）。
 */
public class AiBoxMediaEntity {
    private Long id;
    private String deviceSerial;
    private Long alarmActionId;
    private String mediaType; // IMAGE/VIDEO
    private String originalUrl;
    private Long fileObjectId;
    private String publicUrl;
    private String sha256;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getDeviceSerial() {
        return deviceSerial;
    }

    public void setDeviceSerial(String deviceSerial) {
        this.deviceSerial = deviceSerial;
    }

    public Long getAlarmActionId() {
        return alarmActionId;
    }

    public void setAlarmActionId(Long alarmActionId) {
        this.alarmActionId = alarmActionId;
    }

    public String getMediaType() {
        return mediaType;
    }

    public void setMediaType(String mediaType) {
        this.mediaType = mediaType;
    }

    public String getOriginalUrl() {
        return originalUrl;
    }

    public void setOriginalUrl(String originalUrl) {
        this.originalUrl = originalUrl;
    }

    public Long getFileObjectId() {
        return fileObjectId;
    }

    public void setFileObjectId(Long fileObjectId) {
        this.fileObjectId = fileObjectId;
    }

    public String getPublicUrl() {
        return publicUrl;
    }

    public void setPublicUrl(String publicUrl) {
        this.publicUrl = publicUrl;
    }

    public String getSha256() {
        return sha256;
    }

    public void setSha256(String sha256) {
        this.sha256 = sha256;
    }
}

