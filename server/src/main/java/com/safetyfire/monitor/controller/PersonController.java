package com.safetyfire.monitor.controller;

import com.safetyfire.monitor.common.ApiResponse;
import com.safetyfire.monitor.common.PageResponse;
import com.safetyfire.monitor.config.Audit;
import com.safetyfire.monitor.domain.dto.PersonCreateRequest;
import com.safetyfire.monitor.domain.dto.PersonUpdateRequest;
import com.safetyfire.monitor.domain.vo.PersonVO;
import com.safetyfire.monitor.service.PersonService;
import jakarta.validation.Valid;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

/**
 * 人员档案接口（FR-02）。
 */
@Validated
@RestController
@RequestMapping("/api/v1/people")
public class PersonController {
    private final PersonService personService;

    public PersonController(PersonService personService) {
        this.personService = personService;
    }

    @GetMapping
    @PreAuthorize("hasAuthority('person:manage')")
    public ApiResponse<PageResponse<PersonVO>> list(
            @RequestParam(required = false) String keyword,
            @RequestParam(defaultValue = "1") @Min(1) int page,
            @RequestParam(defaultValue = "20") @Min(1) @Max(200) int pageSize
    ) {
        return ApiResponse.ok(personService.list(keyword, page, pageSize));
    }

    @PostMapping
    @PreAuthorize("hasAuthority('person:manage')")
    @Audit(action = "person.create")
    public ApiResponse<Long> create(@Valid @RequestBody PersonCreateRequest req) {
        return ApiResponse.ok(personService.create(req));
    }

    @PutMapping
    @PreAuthorize("hasAuthority('person:manage')")
    @Audit(action = "person.update")
    public ApiResponse<Void> update(@Valid @RequestBody PersonUpdateRequest req) {
        personService.update(req);
        return ApiResponse.ok(null);
    }

    @DeleteMapping("/{id}")
    @PreAuthorize("hasAuthority('person:manage')")
    @Audit(action = "person.delete")
    public ApiResponse<Void> delete(@PathVariable Long id) {
        personService.delete(id);
        return ApiResponse.ok(null);
    }
}

