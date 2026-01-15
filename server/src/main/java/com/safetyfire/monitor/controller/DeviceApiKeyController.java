package com.safetyfire.monitor.controller;

import com.safetyfire.monitor.common.ApiResponse;
import com.safetyfire.monitor.domain.dto.DeviceApiKeyCreateRequest;
import com.safetyfire.monitor.domain.dto.DeviceApiKeyUpdateRequest;
import com.safetyfire.monitor.domain.vo.DeviceApiKeyVO;
import com.safetyfire.monitor.service.DeviceApiKeyService;
import jakarta.validation.Valid;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * 设备API密钥管理接口。
 */
@RestController
@RequestMapping("/api/v1/device-api-keys")
@PreAuthorize("hasAnyRole('ADMIN', 'SUPER_ADMIN')")
public class DeviceApiKeyController {
    private final DeviceApiKeyService deviceApiKeyService;

    public DeviceApiKeyController(DeviceApiKeyService deviceApiKeyService) {
        this.deviceApiKeyService = deviceApiKeyService;
    }

    /**
     * 查询所有API密钥列表。
     */
    @GetMapping
    public ApiResponse<List<DeviceApiKeyVO>> list() {
        return ApiResponse.ok(deviceApiKeyService.list());
    }

    /**
     * 创建API密钥。
     */
    @PostMapping
    public ApiResponse<Long> create(@Valid @RequestBody DeviceApiKeyCreateRequest req) {
        Long id = deviceApiKeyService.create(req);
        return ApiResponse.ok(id);
    }

    /**
     * 更新API密钥。
     */
    @PutMapping
    public ApiResponse<Void> update(@Valid @RequestBody DeviceApiKeyUpdateRequest req) {
        deviceApiKeyService.update(req);
        return ApiResponse.ok(null);
    }

    /**
     * 删除API密钥。
     */
    @DeleteMapping("/{id}")
    public ApiResponse<Void> delete(@PathVariable Long id) {
        deviceApiKeyService.delete(id);
        return ApiResponse.ok(null);
    }
}
