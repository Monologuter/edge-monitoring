package com.safetyfire.monitor.service;

import com.safetyfire.monitor.common.PageResponse;
import com.safetyfire.monitor.domain.dto.CarInoutIngestRequest;
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
            ingestService.ingestAlarm("非法入侵-车辆未录入", "ACTIVE", req.inOutTime(),
                    "BARRIER_" + req.inOutState(), req.imageFileId() != null ? "车牌:" + req.licensePlateNumber() : null);
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

    public PageResponse<CarInoutRecordVO> list(int page, int pageSize) {
        int offset = (page - 1) * pageSize;
        List<String> scope = dataScopeService.currentCompanyCodesOrAll();
        List<CarInoutRecordEntity> list = mapper.listByCompanyCodes(scope, offset, pageSize);
        long total = mapper.countByCompanyCodes(scope);
        return new PageResponse<>(list.stream().map(this::toVo).toList(), page, pageSize, total);
    }

    private CarInoutRecordVO toVo(CarInoutRecordEntity e) {
        return new CarInoutRecordVO(e.getId(), e.getCompanyCode(), e.getLicensePlateNumber(), e.getCarType(), e.getInOutState(), e.getInOutTime(), e.getImageFileId());
    }
}
