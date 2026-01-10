package com.safetyfire.monitor.controller;

import com.safetyfire.monitor.common.ApiResponse;
import com.safetyfire.monitor.common.PageResponse;
import com.safetyfire.monitor.config.Audit;
import com.safetyfire.monitor.domain.dto.StoreCreateRequest;
import com.safetyfire.monitor.domain.dto.StoreUpdateRequest;
import com.safetyfire.monitor.domain.vo.StoreVO;
import com.safetyfire.monitor.service.StoreService;
import jakarta.validation.Valid;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

/**
 * 仓库接口（FR-04）。
 */
@Validated
@RestController
@RequestMapping("/api/v1/stores")
public class StoreController {
    private final StoreService storeService;

    public StoreController(StoreService storeService) {
        this.storeService = storeService;
    }

    @GetMapping
    @PreAuthorize("hasAuthority('store:manage')")
    public ApiResponse<PageResponse<StoreVO>> list(
            @RequestParam(required = false) String keyword,
            @RequestParam(defaultValue = "1") @Min(1) int page,
            @RequestParam(defaultValue = "20") @Min(1) @Max(200) int pageSize
    ) {
        return ApiResponse.ok(storeService.list(keyword, page, pageSize));
    }

    @PostMapping
    @PreAuthorize("hasAuthority('store:manage')")
    @Audit(action = "store.create")
    public ApiResponse<Long> create(@Valid @RequestBody StoreCreateRequest req) {
        return ApiResponse.ok(storeService.create(req));
    }

    @PutMapping
    @PreAuthorize("hasAuthority('store:manage')")
    @Audit(action = "store.update")
    public ApiResponse<Void> update(@Valid @RequestBody StoreUpdateRequest req) {
        storeService.update(req);
        return ApiResponse.ok(null);
    }

    @DeleteMapping("/{id}")
    @PreAuthorize("hasAuthority('store:manage')")
    @Audit(action = "store.delete")
    public ApiResponse<Void> delete(@PathVariable Long id) {
        storeService.delete(id);
        return ApiResponse.ok(null);
    }
}

