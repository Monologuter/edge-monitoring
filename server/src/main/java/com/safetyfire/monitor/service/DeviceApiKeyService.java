package com.safetyfire.monitor.service;

import com.safetyfire.monitor.common.PageResponse;
import com.safetyfire.monitor.domain.dto.DeviceApiKeyCreateRequest;
import com.safetyfire.monitor.domain.dto.DeviceApiKeyUpdateRequest;
import com.safetyfire.monitor.domain.entity.DeviceApiKeyEntity;
import com.safetyfire.monitor.domain.vo.DeviceApiKeyVO;
import com.safetyfire.monitor.mapper.DeviceApiKeyMapper;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.stream.Collectors;

/**
 * 设备API密钥管理服务。
 */
@Service
public class DeviceApiKeyService {
    private final DeviceApiKeyMapper deviceApiKeyMapper;

    public DeviceApiKeyService(DeviceApiKeyMapper deviceApiKeyMapper) {
        this.deviceApiKeyMapper = deviceApiKeyMapper;
    }

    /**
     * 查询所有API密钥列表。
     */
    public List<DeviceApiKeyVO> list() {
        return deviceApiKeyMapper.findAll().stream()
                .map(this::toVO)
                .collect(Collectors.toList());
    }

    /**
     * 创建API密钥。
     */
    @Transactional
    public Long create(DeviceApiKeyCreateRequest req) {
        // 检查apiKey是否已存在
        DeviceApiKeyEntity existing = deviceApiKeyMapper.findByApiKey(req.apiKey());
        if (existing != null) {
            throw new IllegalArgumentException("API Key已存在");
        }

        DeviceApiKeyEntity entity = new DeviceApiKeyEntity();
        entity.setApiKey(req.apiKey());
        entity.setApiSecret(req.apiSecret());
        entity.setEnabled(req.enabled());
        entity.setAllowedIps(req.allowedIps());
        entity.setRateLimitPerMinute(req.rateLimitPerMinute() != null ? req.rateLimitPerMinute() : 60);

        deviceApiKeyMapper.insert(entity);
        return entity.getId();
    }

    /**
     * 更新API密钥。
     */
    @Transactional
    public void update(DeviceApiKeyUpdateRequest req) {
        DeviceApiKeyEntity entity = deviceApiKeyMapper.findById(req.id());
        if (entity == null) {
            throw new IllegalArgumentException("API密钥不存在");
        }

        if (req.apiSecret() != null) {
            entity.setApiSecret(req.apiSecret());
        }
        if (req.enabled() != null) {
            entity.setEnabled(req.enabled());
        }
        if (req.allowedIps() != null) {
            entity.setAllowedIps(req.allowedIps());
        }
        if (req.rateLimitPerMinute() != null) {
            entity.setRateLimitPerMinute(req.rateLimitPerMinute());
        }

        deviceApiKeyMapper.update(entity);
    }

    /**
     * 删除API密钥。
     */
    @Transactional
    public void delete(Long id) {
        DeviceApiKeyEntity entity = deviceApiKeyMapper.findById(id);
        if (entity == null) {
            throw new IllegalArgumentException("API密钥不存在");
        }
        deviceApiKeyMapper.deleteById(id);
    }

    private DeviceApiKeyVO toVO(DeviceApiKeyEntity entity) {
        // 隐藏完整密钥，只显示前8位
        String maskedSecret = entity.getApiSecret() != null && entity.getApiSecret().length() > 8
                ? entity.getApiSecret().substring(0, 8) + "****"
                : "****";
        return new DeviceApiKeyVO(
                entity.getId(),
                entity.getApiKey(),
                maskedSecret,
                entity.getEnabled(),
                entity.getAllowedIps(),
                entity.getRateLimitPerMinute()
        );
    }
}
