package com.safetyfire.monitor.controller;

import com.safetyfire.monitor.common.ApiResponse;
import com.safetyfire.monitor.common.PageResponse;
import com.safetyfire.monitor.config.Audit;
import com.safetyfire.monitor.domain.dto.CameraCreateRequest;
import com.safetyfire.monitor.domain.dto.CameraUpdateRequest;
import com.safetyfire.monitor.domain.vo.CameraVO;
import com.safetyfire.monitor.service.CameraService;
import jakarta.validation.Valid;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

/**
 * 摄像头接入接口（FR-09）。
 */
@Validated
@RestController
@RequestMapping("/api/v1/cameras")
public class CameraController {
    private final CameraService cameraService;

    public CameraController(CameraService cameraService) {
        this.cameraService = cameraService;
    }

    @GetMapping
    @PreAuthorize("hasAuthority('camera:manage')")
    public ApiResponse<PageResponse<CameraVO>> list(
            @RequestParam(required = false) String keyword,
            @RequestParam(defaultValue = "1") @Min(1) int page,
            @RequestParam(defaultValue = "20") @Min(1) @Max(200) int pageSize
    ) {
        return ApiResponse.ok(cameraService.list(keyword, page, pageSize));
    }

    @PostMapping
    @PreAuthorize("hasAuthority('camera:manage')")
    @Audit(action = "camera.create")
    public ApiResponse<Long> create(@Valid @RequestBody CameraCreateRequest req) {
        return ApiResponse.ok(cameraService.create(req));
    }

    @PutMapping
    @PreAuthorize("hasAuthority('camera:manage')")
    @Audit(action = "camera.update")
    public ApiResponse<Void> update(@Valid @RequestBody CameraUpdateRequest req) {
        cameraService.update(req);
        return ApiResponse.ok(null);
    }

    @DeleteMapping("/{id}")
    @PreAuthorize("hasAuthority('camera:manage')")
    @Audit(action = "camera.delete")
    public ApiResponse<Void> delete(@PathVariable Long id) {
        cameraService.delete(id);
        return ApiResponse.ok(null);
    }
}

