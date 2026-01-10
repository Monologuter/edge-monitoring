package com.safetyfire.monitor.controller;

import com.safetyfire.monitor.common.ApiResponse;
import com.safetyfire.monitor.common.PageResponse;
import com.safetyfire.monitor.config.Audit;
import com.safetyfire.monitor.domain.dto.DeviceCreateRequest;
import com.safetyfire.monitor.domain.dto.DeviceUpdateRequest;
import com.safetyfire.monitor.domain.vo.DeviceVO;
import com.safetyfire.monitor.service.DeviceService;
import jakarta.validation.Valid;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.bind.annotation.*;

/**
 * 设备接口。
 */
@Validated
@RestController
@RequestMapping("/api/v1/devices")
public class DeviceController {
    private final DeviceService deviceService;

    public DeviceController(DeviceService deviceService) {
        this.deviceService = deviceService;
    }

    @GetMapping
    @PreAuthorize("hasAuthority('device:manage')")
    public ApiResponse<PageResponse<DeviceVO>> list(
            @RequestParam(defaultValue = "1") @Min(1) int page,
            @RequestParam(defaultValue = "20") @Min(1) @Max(200) int pageSize
    ) {
        return ApiResponse.ok(deviceService.list(page, pageSize));
    }

    @PostMapping
    @PreAuthorize("hasAuthority('device:manage')")
    @Audit(action = "device.create")
    public ApiResponse<Long> create(@Valid @RequestBody DeviceCreateRequest req) {
        return ApiResponse.ok(deviceService.create(req));
    }

    @PutMapping
    @PreAuthorize("hasAuthority('device:manage')")
    @Audit(action = "device.update")
    public ApiResponse<Void> update(@Valid @RequestBody DeviceUpdateRequest req) {
        deviceService.update(req);
        return ApiResponse.ok(null);
    }

    @DeleteMapping("/{id}")
    @PreAuthorize("hasAuthority('device:manage')")
    @Audit(action = "device.delete")
    public ApiResponse<Void> delete(@PathVariable Long id) {
        deviceService.delete(id);
        return ApiResponse.ok(null);
    }
}
