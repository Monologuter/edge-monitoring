package com.safetyfire.monitor.controller;

import com.safetyfire.monitor.common.ApiResponse;
import com.safetyfire.monitor.config.Audit;
import com.safetyfire.monitor.domain.dto.ProvincialFeedbackResponse;
import com.safetyfire.monitor.domain.dto.ProvincialUnitInfoDTO;
import com.safetyfire.monitor.domain.dto.ProvincialWarningReportDTO;
import com.safetyfire.monitor.service.ProvincialReportService;
import jakarta.validation.Valid;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * 省厅数据上报接口。
 */
@RestController
@RequestMapping("/api/v1/provincial/report")
public class ProvincialReportController {
    private final ProvincialReportService provincialReportService;

    public ProvincialReportController(ProvincialReportService provincialReportService) {
        this.provincialReportService = provincialReportService;
    }

    /**
     * 上报企业基础信息。
     */
    @PostMapping("/unit")
    @PreAuthorize("hasAuthority('provincial:report')")
    @Audit(action = "provincial.report.unit")
    public ApiResponse<Boolean> reportUnitInfo(@Valid @RequestBody ProvincialUnitInfoDTO unitInfo) {
        boolean success = provincialReportService.reportUnitInfo(unitInfo);
        return ApiResponse.ok(success);
    }

    /**
     * 上报危险品五停预警信息。
     */
    @PostMapping("/warning")
    @PreAuthorize("hasAuthority('provincial:report')")
    @Audit(action = "provincial.report.warning")
    public ApiResponse<Boolean> reportWarning(@Valid @RequestBody ProvincialWarningReportDTO warning) {
        boolean success = provincialReportService.reportWarning(warning);
        return ApiResponse.ok(success);
    }

    /**
     * 拉取预警反馈。
     */
    @GetMapping("/feedback/{warningId}")
    @PreAuthorize("hasAuthority('provincial:report')")
    public ApiResponse<List<ProvincialFeedbackResponse>> fetchFeedback(@PathVariable String warningId) {
        List<ProvincialFeedbackResponse> feedback = provincialReportService.fetchFeedback(warningId);
        return ApiResponse.ok(feedback);
    }

    /**
     * 手动触发企业信息同步。
     */
    @PostMapping("/sync/unit")
    @PreAuthorize("hasAuthority('provincial:manage')")
    @Audit(action = "provincial.sync.unit")
    public ApiResponse<Boolean> syncUnitInfo() {
        // 从数据库读取企业信息并上报
        // TODO: 实现企业信息同步逻辑
        return ApiResponse.ok(true);
    }
}
