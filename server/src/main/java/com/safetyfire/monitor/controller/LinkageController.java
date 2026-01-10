package com.safetyfire.monitor.controller;

import com.safetyfire.monitor.common.ApiResponse;
import com.safetyfire.monitor.common.PageResponse;
import com.safetyfire.monitor.config.Audit;
import com.safetyfire.monitor.domain.dto.LinkageCreateRequest;
import com.safetyfire.monitor.domain.entity.LinkageEventEntity;
import com.safetyfire.monitor.service.LinkageService;
import jakarta.validation.Valid;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

/**
 * 联动接口（FR-11）。
 */
@Validated
@RestController
@RequestMapping("/api/v1/linkage")
public class LinkageController {
    private final LinkageService linkageService;

    public LinkageController(LinkageService linkageService) {
        this.linkageService = linkageService;
    }

    @GetMapping("/events")
    @PreAuthorize("hasAuthority('alarm:manage')")
    public ApiResponse<PageResponse<LinkageEventEntity>> list(
            @RequestParam(required = false) String status,
            @RequestParam(defaultValue = "1") @Min(1) int page,
            @RequestParam(defaultValue = "20") @Min(1) @Max(200) int pageSize
    ) {
        return ApiResponse.ok(linkageService.list(status, page, pageSize));
    }

    @PostMapping("/events")
    @PreAuthorize("hasAuthority('alarm:manage')")
    @Audit(action = "linkage.create")
    public ApiResponse<Long> create(@Valid @RequestBody LinkageCreateRequest req) {
        return ApiResponse.ok(linkageService.create(req));
    }
}

