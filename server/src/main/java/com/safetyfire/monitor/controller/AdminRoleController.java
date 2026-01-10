package com.safetyfire.monitor.controller;

import com.safetyfire.monitor.common.ApiResponse;
import com.safetyfire.monitor.common.PageResponse;
import com.safetyfire.monitor.config.Audit;
import com.safetyfire.monitor.domain.dto.AdminRoleUpsertRequest;
import com.safetyfire.monitor.domain.vo.AdminRoleVO;
import com.safetyfire.monitor.domain.vo.MenuVO;
import com.safetyfire.monitor.service.AdminRoleService;
import jakarta.validation.Valid;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * 管理后台：角色管理接口。
 */
@Validated
@RestController
@RequestMapping("/api/v1/admin/roles")
@PreAuthorize("hasAuthority('admin:manage')")
public class AdminRoleController {
    private final AdminRoleService adminRoleService;

    public AdminRoleController(AdminRoleService adminRoleService) {
        this.adminRoleService = adminRoleService;
    }

    @GetMapping
    public ApiResponse<PageResponse<AdminRoleVO>> list(
            @RequestParam(required = false) String keyword,
            @RequestParam(defaultValue = "1") @Min(1) int page,
            @RequestParam(defaultValue = "20") @Min(1) @Max(200) int pageSize
    ) {
        return ApiResponse.ok(adminRoleService.list(keyword, page, pageSize));
    }

    @PostMapping
    @Audit(action = "admin.role.upsert")
    public ApiResponse<Long> upsert(@Valid @RequestBody AdminRoleUpsertRequest req) {
        return ApiResponse.ok(adminRoleService.upsert(req));
    }

    @DeleteMapping("/{id}")
    @Audit(action = "admin.role.delete")
    public ApiResponse<Void> delete(@PathVariable Long id) {
        adminRoleService.delete(id);
        return ApiResponse.ok(null);
    }

    @GetMapping("/meta/permissions")
    public ApiResponse<List<String>> permissions() {
        return ApiResponse.ok(adminRoleService.listAllPermissionKeys());
    }

    @GetMapping("/meta/menus")
    public ApiResponse<List<MenuVO>> menus() {
        return ApiResponse.ok(adminRoleService.listAllMenus());
    }
}

