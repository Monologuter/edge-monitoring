package com.safetyfire.monitor.controller;

import com.safetyfire.monitor.common.ApiResponse;
import com.safetyfire.monitor.common.PageResponse;
import com.safetyfire.monitor.config.Audit;
import com.safetyfire.monitor.domain.dto.CompanyCreateRequest;
import com.safetyfire.monitor.domain.dto.CompanyUpdateRequest;
import com.safetyfire.monitor.domain.vo.CompanyVO;
import com.safetyfire.monitor.service.CompanyService;
import jakarta.validation.Valid;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

/**
 * 企业档案接口（FR-01）。
 */
@Validated
@RestController
@RequestMapping("/api/v1/companies")
public class CompanyController {
    private final CompanyService companyService;

    public CompanyController(CompanyService companyService) {
        this.companyService = companyService;
    }

    @GetMapping
    @PreAuthorize("hasAuthority('company:manage')")
    public ApiResponse<PageResponse<CompanyVO>> list(
            @RequestParam(required = false) String keyword,
            @RequestParam(defaultValue = "1") @Min(1) int page,
            @RequestParam(defaultValue = "20") @Min(1) @Max(200) int pageSize
    ) {
        return ApiResponse.ok(companyService.list(keyword, page, pageSize));
    }

    @GetMapping("/{id}")
    @PreAuthorize("hasAuthority('company:manage')")
    public ApiResponse<CompanyVO> get(@PathVariable Long id) {
        return ApiResponse.ok(companyService.get(id));
    }

    @PostMapping
    @PreAuthorize("hasAuthority('admin:manage')")
    @Audit(action = "company.create")
    public ApiResponse<Long> create(@Valid @RequestBody CompanyCreateRequest req) {
        return ApiResponse.ok(companyService.create(req));
    }

    @PutMapping
    @PreAuthorize("hasAuthority('admin:manage')")
    @Audit(action = "company.update")
    public ApiResponse<Void> update(@Valid @RequestBody CompanyUpdateRequest req) {
        companyService.update(req);
        return ApiResponse.ok(null);
    }

    @DeleteMapping("/{id}")
    @PreAuthorize("hasAuthority('admin:manage')")
    @Audit(action = "company.delete")
    public ApiResponse<Void> delete(@PathVariable Long id) {
        companyService.delete(id);
        return ApiResponse.ok(null);
    }
}
