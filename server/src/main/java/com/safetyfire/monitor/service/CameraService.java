package com.safetyfire.monitor.service;

import com.safetyfire.monitor.common.BizException;
import com.safetyfire.monitor.common.ErrorCode;
import com.safetyfire.monitor.common.PageResponse;
import com.safetyfire.monitor.domain.dto.CameraCreateRequest;
import com.safetyfire.monitor.domain.dto.CameraUpdateRequest;
import com.safetyfire.monitor.domain.entity.CameraEntity;
import com.safetyfire.monitor.domain.vo.CameraVO;
import com.safetyfire.monitor.mapper.CameraMapper;
import com.safetyfire.monitor.security.DataScopeService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

/**
 * 摄像头服务（FR-09）。
 */
@Service
public class CameraService {
    private final CameraMapper cameraMapper;
    private final DataScopeService dataScopeService;

    public CameraService(CameraMapper cameraMapper, DataScopeService dataScopeService) {
        this.cameraMapper = cameraMapper;
        this.dataScopeService = dataScopeService;
    }

    @Transactional
    public Long create(CameraCreateRequest req) {
        if (req.companyCode() != null && !req.companyCode().isBlank()) {
            dataScopeService.assertCompanyAllowed(req.companyCode());
        }
        if (cameraMapper.findByCameraCode(req.cameraCode()) != null) {
            throw new BizException(ErrorCode.STATE_CONFLICT, "摄像头编码已存在");
        }
        CameraEntity e = new CameraEntity();
        e.setCompanyCode(req.companyCode());
        e.setCameraCode(req.cameraCode());
        e.setCameraName(req.cameraName());
        e.setStreamUrl(req.streamUrl());
        e.setLocationName(req.locationName());
        e.setStoreNum(req.storeNum());
        e.setStoreroomNum(req.storeroomNum());
        e.setEnabled(req.enabled());
        cameraMapper.insert(e);
        return e.getId();
    }

    @Transactional
    public void update(CameraUpdateRequest req) {
        CameraEntity e = cameraMapper.findById(req.id());
        if (e == null) throw new BizException(ErrorCode.NOT_FOUND, "摄像头不存在");
        if (e.getCompanyCode() != null) {
            dataScopeService.assertCompanyAllowed(e.getCompanyCode());
        }
        if (req.companyCode() != null && !req.companyCode().isBlank()) {
            dataScopeService.assertCompanyAllowed(req.companyCode());
        }
        CameraEntity byCode = cameraMapper.findByCameraCode(req.cameraCode());
        if (byCode != null && !byCode.getId().equals(req.id())) {
            throw new BizException(ErrorCode.STATE_CONFLICT, "摄像头编码已存在");
        }
        e.setCompanyCode(req.companyCode());
        e.setCameraCode(req.cameraCode());
        e.setCameraName(req.cameraName());
        e.setStreamUrl(req.streamUrl());
        e.setLocationName(req.locationName());
        e.setStoreNum(req.storeNum());
        e.setStoreroomNum(req.storeroomNum());
        e.setEnabled(req.enabled());
        cameraMapper.update(e);
    }

    @Transactional
    public void delete(Long id) {
        CameraEntity e = cameraMapper.findById(id);
        if (e == null) throw new BizException(ErrorCode.NOT_FOUND, "摄像头不存在");
        if (e.getCompanyCode() != null) {
            dataScopeService.assertCompanyAllowed(e.getCompanyCode());
        }
        int n = cameraMapper.deleteById(id);
        if (n <= 0) throw new BizException(ErrorCode.NOT_FOUND, "摄像头不存在");
    }

    public PageResponse<CameraVO> list(String keyword, int page, int pageSize) {
        int offset = (page - 1) * pageSize;
        List<String> scope = dataScopeService.currentCompanyCodesOrAll();
        List<CameraEntity> list = cameraMapper.list(keyword, scope, offset, pageSize);
        long total = cameraMapper.count(keyword, scope);
        return new PageResponse<>(list.stream().map(this::toVo).toList(), page, pageSize, total);
    }

    private CameraVO toVo(CameraEntity e) {
        return new CameraVO(e.getId(), e.getCompanyCode(), e.getCameraCode(), e.getCameraName(), e.getStreamUrl(), e.getLocationName(),
                e.getStoreNum(), e.getStoreroomNum(), e.getEnabled());
    }
}
