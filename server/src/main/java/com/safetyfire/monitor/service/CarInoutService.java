package com.safetyfire.monitor.service;

import com.safetyfire.monitor.common.PageResponse;
import com.safetyfire.monitor.domain.dto.CarInoutCreateRequest;
import com.safetyfire.monitor.domain.dto.CarInoutIngestRequest;
import com.safetyfire.monitor.domain.dto.CarInoutUpdateRequest;
import com.safetyfire.monitor.domain.entity.CarInoutRecordEntity;
import com.safetyfire.monitor.domain.vo.CarInoutRecordVO;
import com.safetyfire.monitor.mapper.CarInoutRecordMapper;
import com.safetyfire.monitor.mapper.CarMapper;
import com.safetyfire.monitor.security.DataScopeService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

/**
 * 车辆出入记录服务。
 */
@Service
public class CarInoutService {
    private final CarInoutRecordMapper mapper;
    private final CarMapper carMapper;
    private final DataScopeService dataScopeService;
    private final IngestService ingestService;

    public CarInoutService(CarInoutRecordMapper mapper, CarMapper carMapper, DataScopeService dataScopeService,
                           IngestService ingestService) {
        this.mapper = mapper;
        this.carMapper = carMapper;
        this.dataScopeService = dataScopeService;
        this.ingestService = ingestService;
    }

    @Transactional
    public String ingest(CarInoutIngestRequest req) {
        String companyCode = null;
        var car = carMapper.findByPlate(req.licensePlateNumber());
        if (car != null) {
            companyCode = car.getCompanyCode();
        } else {
            // 车辆不在档案中，触发非法入侵报警
            ingestService.ingestAlarm("非法入侵报警", "ACTIVE", req.inOutTime(),
                    "BARRIER_" + req.inOutState(), req.imageFileId() != null ? "车辆未录入，车牌:" + req.licensePlateNumber() : "车辆未录入");
        }
        CarInoutRecordEntity e = new CarInoutRecordEntity();
        e.setCompanyCode(companyCode);
        e.setLicensePlateNumber(req.licensePlateNumber());
        e.setCarType(req.carType());
        e.setInOutState(req.inOutState());
        e.setInOutTime(req.inOutTime());
        e.setImageFileId(req.imageFileId());
        mapper.insert(e);
        return companyCode;
    }

    @Transactional
    public Long create(CarInoutCreateRequest req) {
        String companyCode = resolveCompanyCode(req.companyCode(), req.licensePlateNumber());
        CarInoutRecordEntity e = new CarInoutRecordEntity();
        e.setCompanyCode(companyCode);
        e.setLicensePlateNumber(req.licensePlateNumber());
        e.setCarType(req.carType());
        e.setInOutState(req.inOutState());
        e.setInOutTime(req.inOutTime());
        e.setImageFileId(req.imageFileId());
        mapper.insert(e);
        return e.getId();
    }

    @Transactional
    public void update(CarInoutUpdateRequest req) {
        CarInoutRecordEntity e = mapper.findById(req.id());
        if (e == null) {
            throw new com.safetyfire.monitor.common.BizException(com.safetyfire.monitor.common.ErrorCode.NOT_FOUND, "记录不存在");
        }
        if (e.getCompanyCode() != null) {
            dataScopeService.assertCompanyAllowed(e.getCompanyCode());
        } else if (dataScopeService.currentCompanyCodesOrAll() != null) {
            throw new com.safetyfire.monitor.common.BizException(com.safetyfire.monitor.common.ErrorCode.FORBIDDEN, "无该记录数据权限");
        }
        String companyCode = resolveCompanyCode(req.companyCode(), req.licensePlateNumber());
        e.setCompanyCode(companyCode);
        e.setLicensePlateNumber(req.licensePlateNumber());
        e.setCarType(req.carType());
        e.setInOutState(req.inOutState());
        e.setInOutTime(req.inOutTime());
        e.setImageFileId(req.imageFileId());
        mapper.update(e);
    }

    @Transactional
    public void delete(Long id) {
        CarInoutRecordEntity e = mapper.findById(id);
        if (e == null) {
            throw new com.safetyfire.monitor.common.BizException(com.safetyfire.monitor.common.ErrorCode.NOT_FOUND, "记录不存在");
        }
        if (e.getCompanyCode() != null) {
            dataScopeService.assertCompanyAllowed(e.getCompanyCode());
        } else if (dataScopeService.currentCompanyCodesOrAll() != null) {
            throw new com.safetyfire.monitor.common.BizException(com.safetyfire.monitor.common.ErrorCode.FORBIDDEN, "无该记录数据权限");
        }
        int n = mapper.deleteById(id);
        if (n <= 0) {
            throw new com.safetyfire.monitor.common.BizException(com.safetyfire.monitor.common.ErrorCode.NOT_FOUND, "记录不存在");
        }
    }

    public PageResponse<CarInoutRecordVO> list(int page, int pageSize) {
        int offset = (page - 1) * pageSize;
        List<String> scope = dataScopeService.currentCompanyCodesOrAll();
        List<CarInoutRecordEntity> list = mapper.listByCompanyCodes(scope, offset, pageSize);
        long total = mapper.countByCompanyCodes(scope);
        return new PageResponse<>(list.stream().map(this::toVo).toList(), page, pageSize, total);
    }

    private CarInoutRecordVO toVo(CarInoutRecordEntity e) {
        return new CarInoutRecordVO(e.getId(), e.getCompanyCode(), e.getLicensePlateNumber(), e.getCarType(), e.getDriverName(), e.getInOutState(), e.getInOutTime(), e.getImageFileId());
    }

    private String resolveCompanyCode(String provided, String plate) {
        if (provided != null && !provided.isBlank()) {
            dataScopeService.assertCompanyAllowed(provided);
            return provided.trim();
        }
        var car = carMapper.findByPlate(plate);
        if (car != null) {
            dataScopeService.assertCompanyAllowed(car.getCompanyCode());
            return car.getCompanyCode();
        }
        if (dataScopeService.currentCompanyCodesOrAll() != null) {
            throw new com.safetyfire.monitor.common.BizException(com.safetyfire.monitor.common.ErrorCode.PARAM_INVALID, "无法解析企业编码");
        }
        return null;
    }
}
