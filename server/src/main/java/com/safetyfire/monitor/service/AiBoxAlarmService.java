package com.safetyfire.monitor.service;

import com.safetyfire.monitor.common.PageResponse;
import com.safetyfire.monitor.domain.dto.AiBoxAlarmIngestRequest;
import com.safetyfire.monitor.domain.entity.AiBoxAlarmEntity;
import com.safetyfire.monitor.domain.vo.AiBoxAlarmVO;
import com.safetyfire.monitor.mapper.AiBoxAlarmMapper;
import com.safetyfire.monitor.mapper.CameraMapper;
import com.safetyfire.monitor.security.DataScopeService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

/**
 * AI盒子告警服务（通道堵塞、超高超重、摄像头遮挡偏移等）。
 */
@Service
public class AiBoxAlarmService {
    private final AiBoxAlarmMapper mapper;
    private final CameraMapper cameraMapper;
    private final IngestService ingestService;
    private final DataScopeService dataScopeService;

    public AiBoxAlarmService(AiBoxAlarmMapper mapper, CameraMapper cameraMapper,
                             IngestService ingestService, DataScopeService dataScopeService) {
        this.mapper = mapper;
        this.cameraMapper = cameraMapper;
        this.ingestService = ingestService;
        this.dataScopeService = dataScopeService;
    }

    /**
     * 接收AI盒子告警数据并入库，同时触发系统告警。
     */
    @Transactional
    public String ingest(AiBoxAlarmIngestRequest req) {
        String companyCode = null;
        if (req.cameraCode() != null && !req.cameraCode().isBlank()) {
            var camera = cameraMapper.findByCameraCode(req.cameraCode());
            if (camera != null) {
                companyCode = camera.getCompanyCode();
            }
        }

        // 保存AI盒子告警记录
        AiBoxAlarmEntity entity = new AiBoxAlarmEntity();
        entity.setCompanyCode(companyCode);
        entity.setAlarmType(req.alarmType());
        entity.setAlarmStatus(req.alarmStatus());
        entity.setWarningTime(req.warningTime());
        entity.setCameraCode(req.cameraCode());
        entity.setAreaCode(req.areaCode());
        entity.setHeight(req.height());
        entity.setWeight(req.weight());
        entity.setDescription(req.description());
        entity.setImageFileId(req.imageFileId());
        mapper.insert(entity);

        // 触发系统告警
        String deviceCode = req.cameraCode();
        String remark = buildRemark(req);
        String alarmFile = req.imageFileId() != null ? "图片ID:" + req.imageFileId() : null;
        ingestService.ingestAlarm(req.alarmType(), req.alarmStatus(), req.warningTime(),
                deviceCode, alarmFile);

        return companyCode;
    }

    private String buildRemark(AiBoxAlarmIngestRequest req) {
        StringBuilder sb = new StringBuilder();
        if (req.height() != null) {
            sb.append("高度: ").append(req.height()).append("米");
        }
        if (req.weight() != null) {
            if (sb.length() > 0) sb.append(", ");
            sb.append("重量: ").append(req.weight()).append("吨");
        }
        if (req.description() != null && !req.description().isBlank()) {
            if (sb.length() > 0) sb.append(", ");
            sb.append(req.description());
        }
        return sb.length() > 0 ? sb.toString() : null;
    }

    public PageResponse<AiBoxAlarmVO> list(int page, int pageSize, String alarmType) {
        int offset = (page - 1) * pageSize;
        List<String> scope = dataScopeService.currentCompanyCodesOrAll();
        List<AiBoxAlarmEntity> list = mapper.listByCompanyCodes(scope, offset, pageSize, alarmType);
        long total = mapper.countByCompanyCodes(scope, alarmType);
        return new PageResponse<>(list.stream().map(this::toVo).toList(), page, pageSize, total);
    }

    private AiBoxAlarmVO toVo(AiBoxAlarmEntity e) {
        return new AiBoxAlarmVO(
                e.getId(),
                e.getCompanyCode(),
                e.getAlarmType(),
                e.getAlarmStatus(),
                e.getWarningTime(),
                e.getCameraCode(),
                e.getAreaCode(),
                e.getHeight(),
                e.getWeight(),
                e.getDescription(),
                e.getImageFileId()
        );
    }
}
