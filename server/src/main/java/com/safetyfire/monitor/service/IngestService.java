package com.safetyfire.monitor.service;

import com.safetyfire.monitor.domain.entity.AlarmEntity;
import com.safetyfire.monitor.domain.entity.DeviceEntity;
import com.safetyfire.monitor.mapper.AlarmMapper;
import com.safetyfire.monitor.mapper.CameraMapper;
import com.safetyfire.monitor.mapper.DeviceMapper;
import com.safetyfire.monitor.mapper.DeviceReadingHourMapper;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * 数据接入服务：用于串口服务器/边缘设备推送数据入库并触发告警闭环。
 */
@Service
public class IngestService {
    private final DeviceMapper deviceMapper;
    private final AlarmMapper alarmMapper;
    private final AlarmService alarmService;
    private final DeviceReadingHourMapper deviceReadingHourMapper;
    private final CameraMapper cameraMapper;

    public IngestService(DeviceMapper deviceMapper, AlarmMapper alarmMapper, AlarmService alarmService, DeviceReadingHourMapper deviceReadingHourMapper,
                         CameraMapper cameraMapper) {
        this.deviceMapper = deviceMapper;
        this.alarmMapper = alarmMapper;
        this.alarmService = alarmService;
        this.deviceReadingHourMapper = deviceReadingHourMapper;
        this.cameraMapper = cameraMapper;
    }

    @Transactional
    public void ingestDeviceReading(String deviceCode, double realValue, long systime) {
        // 设备心跳与在线判定
        deviceMapper.updateHeartbeat(deviceCode, systime);

        // 小时聚合入库（满足“同一设备每小时一条”要求）
        long hourStart = systime - (systime % 3_600_000L);
        deviceReadingHourMapper.upsertSample(deviceCode, hourStart, realValue);

        DeviceEntity device = deviceMapper.findByDeviceCode(deviceCode);
        if (device == null) {
            // 设备不存在时暂不拒绝：避免现场数据丢失，后续可进入“待绑定设备”池
            return;
        }
        Double lower = device.getLowerLimit();
        Double upper = device.getUpperLimit();
        boolean lowAlarm = lower != null && realValue < lower;
        boolean highAlarm = upper != null && realValue > upper;
        if (!lowAlarm && !highAlarm) {
            return;
        }

        AlarmEntity alarm = new AlarmEntity();
        alarm.setCompanyCode(device.getCompanyCode());
        alarm.setAlarmType(highAlarm ? "阈值超上限" : "阈值低于下限");
        alarm.setAlarmStatus("ACTIVE");
        alarm.setWorkflowStatus("NEW");
        alarm.setRiskLevel(highAlarm ? "HIGH" : "MEDIUM");
        alarm.setDeviceCode(deviceCode);
        alarm.setWarningTime(systime);
        alarm.setHandler(null);
        alarm.setRemark("实时值=" + realValue + " " + (device.getUnit() == null ? "" : device.getUnit()));
        alarmMapper.insert(alarm);
        alarmService.created(alarm.getId(), "device", alarm.getRemark());
    }

    @Transactional
    public void ingestAlarm(String alarmType, String alarmStatus, long warningTime, String deviceCode, String alarmFile) {
        String companyCode = null;
        if (deviceCode != null && !deviceCode.isBlank()) {
            DeviceEntity device = deviceMapper.findByDeviceCode(deviceCode);
            if (device != null) {
                companyCode = device.getCompanyCode();
            } else if (cameraMapper != null) {
                var camera = cameraMapper.findByCameraCode(deviceCode);
                if (camera != null) {
                    companyCode = camera.getCompanyCode();
                }
            }
        }
        AlarmEntity alarm = new AlarmEntity();
        alarm.setCompanyCode(companyCode);
        alarm.setAlarmType(alarmType);
        alarm.setAlarmStatus(alarmStatus);
        alarm.setWorkflowStatus("NEW");
        alarm.setRiskLevel("MEDIUM");
        alarm.setDeviceCode(deviceCode);
        alarm.setWarningTime(warningTime);
        alarm.setRemark(alarmFile == null ? null : ("附件=" + alarmFile));
        alarmMapper.insert(alarm);
        alarmService.created(alarm.getId(), "device", alarm.getRemark());
    }

    /**
     * 尝试根据设备/摄像头编码解析企业编码（用于硬件上报日志做数据权限过滤）。
     */
    public String tryResolveCompanyCodeByDeviceOrCamera(String code) {
        if (code == null || code.isBlank()) return null;
        DeviceEntity device = deviceMapper.findByDeviceCode(code);
        if (device != null) return device.getCompanyCode();
        if (cameraMapper != null) {
            var camera = cameraMapper.findByCameraCode(code);
            if (camera != null) return camera.getCompanyCode();
        }
        return null;
    }
}
