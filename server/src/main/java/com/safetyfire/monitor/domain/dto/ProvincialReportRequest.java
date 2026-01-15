package com.safetyfire.monitor.domain.dto;

import java.util.List;

/**
 * 省厅上报请求包装DTO（加密前）。
 */
public class ProvincialReportRequest {
    private String username;
    private String password;
    private String data;
    private Long timestamp;

    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public String getData() {
        return data;
    }

    public void setData(String data) {
        this.data = data;
    }

    public Long getTimestamp() {
        return timestamp;
    }

    public void setTimestamp(Long timestamp) {
        this.timestamp = timestamp;
    }
}
