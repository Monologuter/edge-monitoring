package com.safetyfire.monitor.controller;

import com.safetyfire.monitor.common.ApiResponse;
import com.safetyfire.monitor.common.PageResponse;
import com.safetyfire.monitor.domain.dto.CarInoutCreateRequest;
import com.safetyfire.monitor.domain.dto.CarInoutUpdateRequest;
import com.safetyfire.monitor.domain.vo.CarInoutRecordVO;
import com.safetyfire.monitor.service.CarInoutService;
import jakarta.validation.Valid;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

/**
 * 车辆出入记录查询接口（FR-03）。
 */
@Validated
@RestController
@RequestMapping("/api/v1/car-inout")
public class CarInoutController {
    private final CarInoutService service;

    public CarInoutController(CarInoutService service) {
        this.service = service;
    }

    @GetMapping
    @PreAuthorize("hasAuthority('car:manage')")
    public ApiResponse<PageResponse<CarInoutRecordVO>> list(
            @RequestParam(defaultValue = "1") @Min(1) int page,
            @RequestParam(defaultValue = "20") @Min(1) @Max(200) int pageSize
    ) {
        return ApiResponse.ok(service.list(page, pageSize));
    }

    @PostMapping
    @PreAuthorize("hasAuthority('car:manage')")
    public ApiResponse<Long> create(@Valid @RequestBody CarInoutCreateRequest req) {
        return ApiResponse.ok(service.create(req));
    }

    @PutMapping
    @PreAuthorize("hasAuthority('car:manage')")
    public ApiResponse<Void> update(@Valid @RequestBody CarInoutUpdateRequest req) {
        service.update(req);
        return ApiResponse.ok(null);
    }

    @DeleteMapping("/{id}")
    @PreAuthorize("hasAuthority('car:manage')")
    public ApiResponse<Void> delete(@PathVariable Long id) {
        service.delete(id);
        return ApiResponse.ok(null);
    }
}
