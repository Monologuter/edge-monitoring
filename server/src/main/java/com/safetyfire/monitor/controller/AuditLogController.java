package com.safetyfire.monitor.controller;

import com.safetyfire.monitor.common.ApiResponse;
import com.safetyfire.monitor.common.PageResponse;
import com.safetyfire.monitor.domain.entity.OperationAuditLogEntity;
import com.safetyfire.monitor.mapper.OperationAuditLogMapper;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * 审计日志接口（只读）。
 */
@Validated
@RestController
@RequestMapping("/api/v1/audit-logs")
public class AuditLogController {
    private final OperationAuditLogMapper mapper;

    public AuditLogController(OperationAuditLogMapper mapper) {
        this.mapper = mapper;
    }

    @GetMapping
    @PreAuthorize("hasAuthority('audit:read')")
    public ApiResponse<PageResponse<OperationAuditLogEntity>> list(
            @RequestParam(defaultValue = "1") @Min(1) int page,
            @RequestParam(defaultValue = "20") @Min(1) @Max(200) int pageSize
    ) {
        int offset = (page - 1) * pageSize;
        List<OperationAuditLogEntity> list = mapper.list(offset, pageSize);
        long total = mapper.count();
        return ApiResponse.ok(new PageResponse<>(list, page, pageSize, total));
    }
}

