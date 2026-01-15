package com.safetyfire.monitor.config;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

/**
 * 外部系统链接配置。
 */
@Component
@ConfigurationProperties(prefix = "app.external")
public class ExternalSystemProperties {
    /**
     * 人员定位系统地址。
     */
    private String personLocationUrl;

    public String getPersonLocationUrl() {
        return personLocationUrl;
    }

    public void setPersonLocationUrl(String personLocationUrl) {
        this.personLocationUrl = personLocationUrl;
    }
}
