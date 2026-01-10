package com.safetyfire.monitor.service;

import com.safetyfire.monitor.common.BizException;
import com.safetyfire.monitor.common.ErrorCode;
import com.safetyfire.monitor.common.PageResponse;
import com.safetyfire.monitor.domain.dto.CarCreateRequest;
import com.safetyfire.monitor.domain.dto.CarUpdateRequest;
import com.safetyfire.monitor.domain.entity.CarEntity;
import com.safetyfire.monitor.domain.vo.CarVO;
import com.safetyfire.monitor.mapper.CarMapper;
import com.safetyfire.monitor.security.DataScopeService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

/**
 * 车辆服务：档案维护（FR-03）。
 */
@Service
public class CarService {
    private final CarMapper carMapper;
    private final DataScopeService dataScopeService;

    public CarService(CarMapper carMapper, DataScopeService dataScopeService) {
        this.carMapper = carMapper;
        this.dataScopeService = dataScopeService;
    }

    @Transactional
    public Long create(CarCreateRequest req) {
        dataScopeService.assertCompanyAllowed(req.companyCode());
        if (carMapper.findByPlate(req.licensePlateNumber()) != null) {
            throw new BizException(ErrorCode.STATE_CONFLICT, "车牌号已存在");
        }
        CarEntity e = new CarEntity();
        e.setCompanyCode(req.companyCode());
        e.setLicensePlateNumber(req.licensePlateNumber());
        e.setCarType(req.carType());
        carMapper.insert(e);
        return e.getId();
    }

    @Transactional
    public void update(CarUpdateRequest req) {
        CarEntity e = carMapper.findById(req.id());
        if (e == null) throw new BizException(ErrorCode.NOT_FOUND, "车辆不存在");
        dataScopeService.assertCompanyAllowed(e.getCompanyCode());
        dataScopeService.assertCompanyAllowed(req.companyCode());
        CarEntity byPlate = carMapper.findByPlate(req.licensePlateNumber());
        if (byPlate != null && !byPlate.getId().equals(req.id())) {
            throw new BizException(ErrorCode.STATE_CONFLICT, "车牌号已存在");
        }
        e.setCompanyCode(req.companyCode());
        e.setLicensePlateNumber(req.licensePlateNumber());
        e.setCarType(req.carType());
        carMapper.update(e);
    }

    @Transactional
    public void delete(Long id) {
        CarEntity e = carMapper.findById(id);
        if (e == null) throw new BizException(ErrorCode.NOT_FOUND, "车辆不存在");
        dataScopeService.assertCompanyAllowed(e.getCompanyCode());
        int n = carMapper.deleteById(id);
        if (n <= 0) throw new BizException(ErrorCode.NOT_FOUND, "车辆不存在");
    }

    public PageResponse<CarVO> list(String keyword, int page, int pageSize) {
        int offset = (page - 1) * pageSize;
        List<String> scope = dataScopeService.currentCompanyCodesOrAll();
        List<CarEntity> list = carMapper.list(keyword, scope, offset, pageSize);
        long total = carMapper.count(keyword, scope);
        return new PageResponse<>(list.stream().map(this::toVo).toList(), page, pageSize, total);
    }

    private CarVO toVo(CarEntity e) {
        return new CarVO(e.getId(), e.getCompanyCode(), e.getLicensePlateNumber(), e.getCarType());
    }
}
