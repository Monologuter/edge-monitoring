package com.safetyfire.monitor.service;

import com.safetyfire.monitor.common.PageResponse;
import com.safetyfire.monitor.common.BizException;
import com.safetyfire.monitor.common.ErrorCode;
import com.safetyfire.monitor.domain.dto.DeviceCreateRequest;
import com.safetyfire.monitor.domain.dto.DeviceUpdateRequest;
import com.safetyfire.monitor.domain.entity.DeviceEntity;
import com.safetyfire.monitor.domain.vo.DeviceVO;
import com.safetyfire.monitor.mapper.DeviceMapper;
import com.safetyfire.monitor.security.DataScopeService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

/**
 * 设备服务：列表与基础信息展示（后续可扩展配置、在线监控、心跳等）。
 */
@Service
public class DeviceService {
    private final DeviceMapper deviceMapper;
    private final DataScopeService dataScopeService;

    public DeviceService(DeviceMapper deviceMapper, DataScopeService dataScopeService) {
        this.deviceMapper = deviceMapper;
        this.dataScopeService = dataScopeService;
    }

    @Transactional
    public Long create(DeviceCreateRequest req) {
        if (req.companyCode() != null && !req.companyCode().isBlank()) {
            dataScopeService.assertCompanyAllowed(req.companyCode());
        } else {
            // 非 ADMIN 不允许创建“无企业归属”的设备
            if (dataScopeService.currentCompanyCodesOrAll() != null) {
                throw new BizException(ErrorCode.PARAM_INVALID, "非管理员创建设备必须填写 companyCode");
            }
        }
        if (deviceMapper.findByDeviceCode(req.deviceCode()) != null) {
            throw new BizException(ErrorCode.STATE_CONFLICT, "设备编码已存在");
        }
        DeviceEntity e = new DeviceEntity();
        e.setCompanyCode(req.companyCode());
        e.setDeviceCode(req.deviceCode());
        e.setDeviceName(req.deviceName());
        e.setDeviceType(req.deviceType());
        e.setUnit(req.unit());
        e.setLowerLimit(req.lowerLimit());
        e.setUpperLimit(req.upperLimit());
        e.setLocationName(req.locationName());
        e.setStoreNum(req.storeNum());
        e.setStoreroomNum(req.storeroomNum());
        e.setIpAddress(req.ipAddress());
        e.setAccessUsername(req.accessUsername());
        e.setAccessPassword(req.accessPassword());
        e.setOnlineStatus(0);
        e.setLastHeartbeatTime(null);
        deviceMapper.insert(e);
        return e.getId();
    }

    @Transactional
    public void update(DeviceUpdateRequest req) {
        DeviceEntity e = deviceMapper.findById(req.id());
        if (e == null) throw new BizException(ErrorCode.NOT_FOUND, "设备不存在");
        if (e.getCompanyCode() != null) {
            dataScopeService.assertCompanyAllowed(e.getCompanyCode());
        } else if (dataScopeService.currentCompanyCodesOrAll() != null) {
            throw new BizException(ErrorCode.FORBIDDEN, "无该设备数据权限");
        }
        if (req.companyCode() != null && !req.companyCode().isBlank()) {
            dataScopeService.assertCompanyAllowed(req.companyCode());
        }
        DeviceEntity byCode = deviceMapper.findByDeviceCode(req.deviceCode());
        if (byCode != null && !byCode.getId().equals(req.id())) {
            throw new BizException(ErrorCode.STATE_CONFLICT, "设备编码已存在");
        }
        e.setCompanyCode(req.companyCode());
        e.setDeviceCode(req.deviceCode());
        e.setDeviceName(req.deviceName());
        e.setDeviceType(req.deviceType());
        e.setUnit(req.unit());
        e.setLowerLimit(req.lowerLimit());
        e.setUpperLimit(req.upperLimit());
        e.setLocationName(req.locationName());
        e.setStoreNum(req.storeNum());
        e.setStoreroomNum(req.storeroomNum());
        e.setIpAddress(req.ipAddress());
        e.setAccessUsername(req.accessUsername());
        e.setAccessPassword(req.accessPassword());
        e.setOnlineStatus(req.onlineStatus());
        deviceMapper.update(e);
    }

    @Transactional
    public void delete(Long id) {
        DeviceEntity e = deviceMapper.findById(id);
        if (e == null) throw new BizException(ErrorCode.NOT_FOUND, "设备不存在");
        if (e.getCompanyCode() != null) {
            dataScopeService.assertCompanyAllowed(e.getCompanyCode());
        } else if (dataScopeService.currentCompanyCodesOrAll() != null) {
            throw new BizException(ErrorCode.FORBIDDEN, "无该设备数据权限");
        }
        int n = deviceMapper.deleteById(id);
        if (n <= 0) throw new BizException(ErrorCode.NOT_FOUND, "设备不存在");
    }

    public PageResponse<DeviceVO> list(int page, int pageSize, Integer deviceType) {
        int offset = (page - 1) * pageSize;
        List<String> scope = dataScopeService.currentCompanyCodesOrAll();
        List<DeviceEntity> list = deviceMapper.list(scope, offset, pageSize, deviceType);
        long total = deviceMapper.countAll(scope, deviceType);
        return new PageResponse<>(list.stream().map(this::toVo).toList(), page, pageSize, total);
    }

    private DeviceVO toVo(DeviceEntity e) {
        return new DeviceVO(
                e.getId(),
                e.getCompanyCode(),
                e.getDeviceCode(),
                e.getDeviceName(),
                e.getDeviceType(),
                e.getUnit(),
                e.getLowerLimit(),
                e.getUpperLimit(),
                e.getLocationName(),
                e.getStoreNum(),
                e.getStoreroomNum(),
                e.getIpAddress(),
                e.getAccessUsername(),
                e.getAccessPassword(),
                e.getOnlineStatus()
        );
    }
}
