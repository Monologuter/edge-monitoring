package com.safetyfire.monitor.service;

import com.safetyfire.monitor.domain.entity.AlarmEntity;
import com.safetyfire.monitor.domain.entity.DeviceEntity;
import com.safetyfire.monitor.mapper.AlarmMapper;
import com.safetyfire.monitor.mapper.CameraMapper;
import com.safetyfire.monitor.mapper.DeviceMapper;
import com.safetyfire.monitor.mapper.DeviceReadingHourMapper;
import com.safetyfire.monitor.config.DeviceReadingScaleProperties;
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
    private final DeviceReadingScaleProperties scaleProperties;

    public IngestService(DeviceMapper deviceMapper, AlarmMapper alarmMapper, AlarmService alarmService, DeviceReadingHourMapper deviceReadingHourMapper,
                         CameraMapper cameraMapper, DeviceReadingScaleProperties scaleProperties) {
        this.deviceMapper = deviceMapper;
        this.alarmMapper = alarmMapper;
        this.alarmService = alarmService;
        this.deviceReadingHourMapper = deviceReadingHourMapper;
        this.cameraMapper = cameraMapper;
        this.scaleProperties = scaleProperties;
    }

    @Transactional
    public void ingestDeviceReading(String deviceCode, double realValue, long systime) {
        // 设备心跳与在线判定
        deviceMapper.updateHeartbeat(deviceCode, systime);

        DeviceEntity device = deviceMapper.findByDeviceCode(deviceCode);
        double convertedValue = convertReadingValue(device, realValue);

        // 小时聚合入库（满足“同一设备每小时一条”要求）
        long hourStart = systime - (systime % 3_600_000L);
        deviceReadingHourMapper.upsertSample(deviceCode, hourStart, convertedValue);

        if (device == null) {
            // 设备不存在时暂不拒绝：避免现场数据丢失，后续可进入“待绑定设备”池
            return;
        }
        Double lower = device.getLowerLimit();
        Double upper = device.getUpperLimit();
        boolean lowAlarm = lower != null && convertedValue < lower;
        boolean highAlarm = upper != null && convertedValue > upper;
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
        alarm.setAlarmFile(null);
        alarm.setWarningTime(systime);
        alarm.setHandler(null);
        alarm.setRemark("实时值=" + convertedValue + " " + (device.getUnit() == null ? "" : device.getUnit()));
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
        alarm.setAlarmFile(alarmFile);
        alarm.setWarningTime(warningTime);
        alarm.setRemark(null);
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

    private double convertReadingValue(DeviceEntity device, double rawValue) {
        if (device == null || device.getDeviceType() == null) {
            return rawValue;
        }
        int type = device.getDeviceType();
        // 设备类型：3温度 4湿度 5液位。按协议进行换算（温湿度/10，液位/1000）。
        if (type == 3) {
            double divisor = scaleProperties.getTemperatureDivisor();
            return divisor > 0 ? rawValue / divisor : rawValue;
        }
        if (type == 4) {
            double divisor = scaleProperties.getHumidityDivisor();
            return divisor > 0 ? rawValue / divisor : rawValue;
        }
        if (type == 5) {
            double divisor = scaleProperties.getLevelDivisor();
            return divisor > 0 ? rawValue / divisor : rawValue;
        }
        return rawValue;
    }
}
