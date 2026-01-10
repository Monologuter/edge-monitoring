package com.safetyfire.monitor.service;

import com.safetyfire.monitor.domain.dto.PersonOvercrowdIngestRequest;
import com.safetyfire.monitor.domain.entity.PersonOvercrowdEntity;
import com.safetyfire.monitor.mapper.PersonOvercrowdMapper;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * 人员定位系统超员报警服务。
 */
@Service
public class PersonOvercrowdService {
    private final PersonOvercrowdMapper mapper;
    private final IngestService ingestService;

    public PersonOvercrowdService(PersonOvercrowdMapper mapper, IngestService ingestService) {
        this.mapper = mapper;
        this.ingestService = ingestService;
    }

    /**
     * 接收超员报警数据并入库，同时触发系统告警。
     */
    @Transactional
    public void ingest(PersonOvercrowdIngestRequest req) {
        // 保存超员记录
        PersonOvercrowdEntity entity = new PersonOvercrowdEntity();
        entity.setAreaCode(req.areaCode());
        entity.setAreaName(req.areaName());
        entity.setCurrentCount(req.currentCount());
        entity.setMaxCount(req.maxCount());
        entity.setWarningTime(req.warningTime());
        entity.setCompanyCode(req.companyCode());
        mapper.insert(entity);

        // 触发系统告警
        String remark = String.format("区域: %s, 当前人数: %d, 最大人数: %d",
                req.areaName(), req.currentCount(), req.maxCount());
        ingestService.ingestAlarm("超员报警", "ACTIVE", req.warningTime(),
                "AREA_" + req.areaCode(), remark);
    }
}
