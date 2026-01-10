package com.safetyfire.monitor.controller;

import com.safetyfire.monitor.common.ApiResponse;
import com.safetyfire.monitor.common.PageResponse;
import com.safetyfire.monitor.config.Audit;
import com.safetyfire.monitor.domain.dto.ProvincialFeedbackRequest;
import com.safetyfire.monitor.domain.entity.ProvincialWarningEntity;
import com.safetyfire.monitor.domain.entity.ProvincialWarningFeedbackEntity;
import com.safetyfire.monitor.service.ProvincialService;
import jakarta.validation.Valid;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

/**
 * 省厅对接接口（FR-07/FR-10）。
 */
@Validated
@RestController
@RequestMapping("/api/v1/provincial")
public class ProvincialController {
    private final ProvincialService provincialService;

    public ProvincialController(ProvincialService provincialService) {
        this.provincialService = provincialService;
    }

    @GetMapping("/warnings")
    @PreAuthorize("hasAuthority('provincial:manage')")
    public ApiResponse<PageResponse<ProvincialWarningEntity>> listWarnings(
            @RequestParam(defaultValue = "1") @Min(1) int page,
            @RequestParam(defaultValue = "20") @Min(1) @Max(200) int pageSize
    ) {
        return ApiResponse.ok(provincialService.listWarnings(page, pageSize));
    }

    @GetMapping("/feedback")
    @PreAuthorize("hasAuthority('provincial:manage')")
    public ApiResponse<PageResponse<ProvincialWarningFeedbackEntity>> listFeedback(
            @RequestParam(defaultValue = "1") @Min(1) int page,
            @RequestParam(defaultValue = "20") @Min(1) @Max(200) int pageSize
    ) {
        return ApiResponse.ok(provincialService.listFeedback(page, pageSize));
    }

    @PostMapping("/feedback")
    @PreAuthorize("hasAuthority('provincial:manage')")
    @Audit(action = "provincial.feedback")
    public ApiResponse<Void> submitFeedback(@Valid @RequestBody ProvincialFeedbackRequest req) {
        provincialService.submitFeedback(req);
        return ApiResponse.ok(null);
    }
}

