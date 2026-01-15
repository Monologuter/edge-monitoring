package com.safetyfire.monitor.controller;

import com.safetyfire.monitor.common.ApiResponse;
import com.safetyfire.monitor.common.PageResponse;
import com.safetyfire.monitor.config.Audit;
import com.safetyfire.monitor.domain.dto.StoreroomCreateRequest;
import com.safetyfire.monitor.domain.dto.StoreroomUpdateRequest;
import com.safetyfire.monitor.domain.vo.StoreroomVO;
import com.safetyfire.monitor.domain.vo.ImportResultVO;
import com.safetyfire.monitor.service.StoreroomService;
import jakarta.validation.Valid;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

/**
 * 库房接口（FR-04）。
 */
@Validated
@RestController
@RequestMapping("/api/v1/storerooms")
public class StoreroomController {
    private final StoreroomService storeroomService;

    public StoreroomController(StoreroomService storeroomService) {
        this.storeroomService = storeroomService;
    }

    @GetMapping
    @PreAuthorize("hasAuthority('store:manage')")
    public ApiResponse<PageResponse<StoreroomVO>> list(
            @RequestParam(required = false) String storeNum,
            @RequestParam(required = false) String keyword,
            @RequestParam(defaultValue = "1") @Min(1) int page,
            @RequestParam(defaultValue = "20") @Min(1) @Max(200) int pageSize
    ) {
        return ApiResponse.ok(storeroomService.list(storeNum, keyword, page, pageSize));
    }

    @PostMapping
    @PreAuthorize("hasAuthority('store:manage')")
    @Audit(action = "storeroom.create")
    public ApiResponse<Long> create(@Valid @RequestBody StoreroomCreateRequest req) {
        return ApiResponse.ok(storeroomService.create(req));
    }

    @PostMapping("/import")
    @PreAuthorize("hasAuthority('store:manage')")
    @Audit(action = "storeroom.import")
    public ApiResponse<ImportResultVO> importCsv(@RequestPart("file") MultipartFile file) {
        return ApiResponse.ok(storeroomService.importCsv(file));
    }

    @PutMapping
    @PreAuthorize("hasAuthority('store:manage')")
    @Audit(action = "storeroom.update")
    public ApiResponse<Void> update(@Valid @RequestBody StoreroomUpdateRequest req) {
        storeroomService.update(req);
        return ApiResponse.ok(null);
    }

    @DeleteMapping("/{id}")
    @PreAuthorize("hasAuthority('store:manage')")
    @Audit(action = "storeroom.delete")
    public ApiResponse<Void> delete(@PathVariable Long id) {
        storeroomService.delete(id);
        return ApiResponse.ok(null);
    }
}
