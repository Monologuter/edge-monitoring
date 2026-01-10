package com.safetyfire.monitor.controller;

import com.safetyfire.monitor.common.ApiResponse;
import com.safetyfire.monitor.common.PageResponse;
import com.safetyfire.monitor.domain.entity.HardwareIngestLogEntity;
import com.safetyfire.monitor.service.HardwareIngestLogService;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

/**
 * 硬件上报日志接口：用于后台查看“硬件传递过来的数据”（原始 payload + 解析结果）。
 */
@Validated
@RestController
@RequestMapping("/api/v1/hardware/ingest-logs")
public class HardwareIngestLogController {
    private final HardwareIngestLogService hardwareIngestLogService;

    public HardwareIngestLogController(HardwareIngestLogService hardwareIngestLogService) {
        this.hardwareIngestLogService = hardwareIngestLogService;
    }

    @GetMapping
    @PreAuthorize("hasAuthority('device:manage')")
    public ApiResponse<PageResponse<HardwareIngestLogEntity>> list(
            @RequestParam(required = false) String channel,
            @RequestParam(required = false) String messageType,
            @RequestParam(required = false) String apiKey,
            @RequestParam(required = false) String companyCode,
            @RequestParam(required = false) Integer ok,
            @RequestParam(required = false) String topicLike,
            @RequestParam(defaultValue = "1") @Min(1) int page,
            @RequestParam(defaultValue = "20") @Min(1) @Max(200) int pageSize
    ) {
        return ApiResponse.ok(hardwareIngestLogService.list(channel, messageType, apiKey, companyCode, ok, topicLike, page, pageSize));
    }
}

