package com.safetyfire.monitor.controller;

import com.safetyfire.monitor.common.ApiResponse;
import com.safetyfire.monitor.common.PageResponse;
import com.safetyfire.monitor.config.Audit;
import com.safetyfire.monitor.domain.dto.CarCreateRequest;
import com.safetyfire.monitor.domain.dto.CarUpdateRequest;
import com.safetyfire.monitor.domain.vo.CarVO;
import com.safetyfire.monitor.domain.vo.ImportResultVO;
import com.safetyfire.monitor.service.CarService;
import jakarta.validation.Valid;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

/**
 * 车辆档案接口（FR-03）。
 */
@Validated
@RestController
@RequestMapping("/api/v1/cars")
public class CarController {
    private final CarService carService;

    public CarController(CarService carService) {
        this.carService = carService;
    }

    @GetMapping
    @PreAuthorize("hasAuthority('car:manage')")
    public ApiResponse<PageResponse<CarVO>> list(
            @RequestParam(required = false) String keyword,
            @RequestParam(defaultValue = "1") @Min(1) int page,
            @RequestParam(defaultValue = "20") @Min(1) @Max(200) int pageSize
    ) {
        return ApiResponse.ok(carService.list(keyword, page, pageSize));
    }

    @PostMapping
    @PreAuthorize("hasAuthority('car:manage')")
    @Audit(action = "car.create")
    public ApiResponse<Long> create(@Valid @RequestBody CarCreateRequest req) {
        return ApiResponse.ok(carService.create(req));
    }

    @PostMapping("/import")
    @PreAuthorize("hasAuthority('car:manage')")
    @Audit(action = "car.import")
    public ApiResponse<ImportResultVO> importCsv(@RequestPart("file") MultipartFile file) {
        return ApiResponse.ok(carService.importCsv(file));
    }

    @PutMapping
    @PreAuthorize("hasAuthority('car:manage')")
    @Audit(action = "car.update")
    public ApiResponse<Void> update(@Valid @RequestBody CarUpdateRequest req) {
        carService.update(req);
        return ApiResponse.ok(null);
    }

    @DeleteMapping("/{id}")
    @PreAuthorize("hasAuthority('car:manage')")
    @Audit(action = "car.delete")
    public ApiResponse<Void> delete(@PathVariable Long id) {
        carService.delete(id);
        return ApiResponse.ok(null);
    }
}
