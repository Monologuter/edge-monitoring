package com.safetyfire.monitor.controller;

import com.safetyfire.monitor.common.ApiResponse;
import com.safetyfire.monitor.domain.vo.DeviceReadingHourVO;
import com.safetyfire.monitor.mapper.DeviceReadingQueryMapper;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotBlank;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * 设备采集数据查询（FR-05）。
 */
@Validated
@RestController
@RequestMapping("/api/v1/readings")
public class DeviceReadingController {
    private final DeviceReadingQueryMapper queryMapper;

    public DeviceReadingController(DeviceReadingQueryMapper queryMapper) {
        this.queryMapper = queryMapper;
    }

    @GetMapping("/hourly")
    @PreAuthorize("hasAuthority('device:manage')")
    public ApiResponse<List<DeviceReadingHourVO>> hourly(
            @RequestParam @NotBlank String deviceCode,
            @RequestParam(defaultValue = "24") @Min(1) @Max(168) int hours
    ) {
        long end = System.currentTimeMillis();
        long start = end - hours * 3600_000L;
        return ApiResponse.ok(queryMapper.listHourly(deviceCode, start, end));
    }
}

