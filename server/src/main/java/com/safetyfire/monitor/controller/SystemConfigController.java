package com.safetyfire.monitor.controller;

import com.safetyfire.monitor.common.ApiResponse;
import com.safetyfire.monitor.config.ExternalSystemProperties;
import com.safetyfire.monitor.domain.vo.SystemConfigVO;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * 系统配置接口：为前端提供外部链接等配置。
 */
@RestController
@RequestMapping("/api/v1/system/config")
public class SystemConfigController {
    private final ExternalSystemProperties properties;

    public SystemConfigController(ExternalSystemProperties properties) {
        this.properties = properties;
    }

    @GetMapping
    public ApiResponse<SystemConfigVO> get() {
        return ApiResponse.ok(new SystemConfigVO(properties.getPersonLocationUrl()));
    }
}
