package com.safetyfire.monitor.controller;

import com.safetyfire.monitor.common.ApiResponse;
import com.safetyfire.monitor.common.PageResponse;
import com.safetyfire.monitor.config.Audit;
import com.safetyfire.monitor.domain.dto.AlarmHandleRequest;
import com.safetyfire.monitor.domain.vo.AlarmVO;
import com.safetyfire.monitor.service.AlarmService;
import jakarta.validation.Valid;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

/**
 * 告警中心接口。
 */
@Validated
@RestController
@RequestMapping("/api/v1/alarms")
public class AlarmController {
    private final AlarmService alarmService;

    public AlarmController(AlarmService alarmService) {
        this.alarmService = alarmService;
    }

    @GetMapping
    @PreAuthorize("hasAuthority('alarm:manage')")
    public ApiResponse<PageResponse<AlarmVO>> list(
            @RequestParam(defaultValue = "1") @Min(1) int page,
            @RequestParam(defaultValue = "20") @Min(1) @Max(200) int pageSize,
            @RequestParam(required = false) String status
    ) {
        return ApiResponse.ok(alarmService.list(page, pageSize, status));
    }

    @PostMapping("/handle")
    @PreAuthorize("hasAuthority('alarm:manage')")
    @Audit(action = "alarm.handle")
    public ApiResponse<Void> handle(@Valid @RequestBody AlarmHandleRequest req) {
        alarmService.handle(req.alarmId(), req.action(), req.remark());
        return ApiResponse.ok(null);
    }
}
