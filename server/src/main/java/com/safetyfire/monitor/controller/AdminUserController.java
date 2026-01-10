package com.safetyfire.monitor.controller;

import com.safetyfire.monitor.common.ApiResponse;
import com.safetyfire.monitor.common.PageResponse;
import com.safetyfire.monitor.config.Audit;
import com.safetyfire.monitor.domain.dto.AdminUserCreateRequest;
import com.safetyfire.monitor.domain.dto.AdminUserResetPasswordRequest;
import com.safetyfire.monitor.domain.dto.AdminUserUpdateRequest;
import com.safetyfire.monitor.domain.vo.AdminUserVO;
import com.safetyfire.monitor.service.AdminUserService;
import jakarta.validation.Valid;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

/**
 * 管理后台：用户管理接口。
 */
@Validated
@RestController
@RequestMapping("/api/v1/admin/users")
@PreAuthorize("hasAuthority('admin:manage')")
public class AdminUserController {
    private final AdminUserService adminUserService;

    public AdminUserController(AdminUserService adminUserService) {
        this.adminUserService = adminUserService;
    }

    @GetMapping
    public ApiResponse<PageResponse<AdminUserVO>> list(
            @RequestParam(required = false) String keyword,
            @RequestParam(defaultValue = "1") @Min(1) int page,
            @RequestParam(defaultValue = "20") @Min(1) @Max(200) int pageSize
    ) {
        return ApiResponse.ok(adminUserService.list(keyword, page, pageSize));
    }

    @PostMapping
    @Audit(action = "admin.user.create")
    public ApiResponse<Long> create(@Valid @RequestBody AdminUserCreateRequest req) {
        return ApiResponse.ok(adminUserService.create(req));
    }

    @PutMapping
    @Audit(action = "admin.user.update")
    public ApiResponse<Void> update(@Valid @RequestBody AdminUserUpdateRequest req) {
        adminUserService.update(req);
        return ApiResponse.ok(null);
    }

    @DeleteMapping("/{id}")
    @Audit(action = "admin.user.delete")
    public ApiResponse<Void> delete(@PathVariable Long id) {
        adminUserService.delete(id);
        return ApiResponse.ok(null);
    }

    @PostMapping("/reset-password")
    @Audit(action = "admin.user.resetPassword")
    public ApiResponse<Void> resetPassword(@Valid @RequestBody AdminUserResetPasswordRequest req) {
        adminUserService.resetPassword(req);
        return ApiResponse.ok(null);
    }
}

