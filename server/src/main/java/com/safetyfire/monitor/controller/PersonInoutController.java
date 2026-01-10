package com.safetyfire.monitor.controller;

import com.safetyfire.monitor.common.ApiResponse;
import com.safetyfire.monitor.common.PageResponse;
import com.safetyfire.monitor.domain.vo.PersonInoutRecordVO;
import com.safetyfire.monitor.service.PersonInoutService;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

/**
 * 人员出入记录查询接口（FR-02）。
 */
@Validated
@RestController
@RequestMapping("/api/v1/person-inout")
public class PersonInoutController {
    private final PersonInoutService service;

    public PersonInoutController(PersonInoutService service) {
        this.service = service;
    }

    @GetMapping
    @PreAuthorize("hasAuthority('person:manage')")
    public ApiResponse<PageResponse<PersonInoutRecordVO>> list(
            @RequestParam(defaultValue = "1") @Min(1) int page,
            @RequestParam(defaultValue = "20") @Min(1) @Max(200) int pageSize
    ) {
        return ApiResponse.ok(service.list(page, pageSize));
    }
}

