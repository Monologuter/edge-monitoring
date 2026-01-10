package com.safetyfire.monitor.controller;

import com.safetyfire.monitor.common.ApiResponse;
import com.safetyfire.monitor.common.PageResponse;
import com.safetyfire.monitor.domain.entity.ServerMachineEntity;
import com.safetyfire.monitor.mapper.ServerMachineMapper;
import com.safetyfire.monitor.security.DataScopeService;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/**
 * 企业服务器信息接口。
 */
@Validated
@RestController
@RequestMapping("/api/v1/servers")
public class ServerMachineController {
    private final ServerMachineMapper mapper;
    private final DataScopeService dataScopeService;

    public ServerMachineController(ServerMachineMapper mapper, DataScopeService dataScopeService) {
        this.mapper = mapper;
        this.dataScopeService = dataScopeService;
    }

    @GetMapping
    @PreAuthorize("hasAuthority('device:manage')")
    public ApiResponse<PageResponse<ServerMachineEntity>> list(
            @RequestParam(defaultValue = "1") @Min(1) int page,
            @RequestParam(defaultValue = "20") @Min(1) @Max(200) int pageSize
    ) {
        int offset = (page - 1) * pageSize;
        List<String> scope = dataScopeService.currentCompanyCodesOrAll();
        List<ServerMachineEntity> list = mapper.list(scope, offset, pageSize);
        long total = mapper.count(scope);
        return ApiResponse.ok(new PageResponse<>(list, page, pageSize, total));
    }
}
