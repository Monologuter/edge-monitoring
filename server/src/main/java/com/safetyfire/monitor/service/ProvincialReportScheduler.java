package com.safetyfire.monitor.service;

import com.safetyfire.monitor.domain.dto.ProvincialUnitInfoDTO;
import com.safetyfire.monitor.domain.dto.ProvincialWarningReportDTO;
import com.safetyfire.monitor.mapper.CompanyMapper;
import com.safetyfire.monitor.mapper.DeviceMapper;
import com.safetyfire.monitor.mapper.AlarmMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * 省厅数据上报定时任务。
 */
@Component
public class ProvincialReportScheduler {
    private static final Logger log = LoggerFactory.getLogger(ProvincialReportScheduler.class);

    private final boolean enabled;
    private final ProvincialReportService provincialReportService;
    private final CompanyMapper companyMapper;
    private final DeviceMapper deviceMapper;
    private final AlarmMapper alarmMapper;

    public ProvincialReportScheduler(
            @Value("${app.provincial.report.enabled:false}") boolean enabled,
            ProvincialReportService provincialReportService,
            CompanyMapper companyMapper,
            DeviceMapper deviceMapper,
            AlarmMapper alarmMapper
    ) {
        this.enabled = enabled;
        this.provincialReportService = provincialReportService;
        this.companyMapper = companyMapper;
        this.deviceMapper = deviceMapper;
        this.alarmMapper = alarmMapper;
    }

    /**
     * 定时上报企业基础信息（每天凌晨1点执行）。
     */
    @Scheduled(cron = "0 0 1 * * ?")
    public void reportUnitInfo() {
        if (!enabled) {
            return;
        }

        log.info("开始定时上报企业基础信息");

        try {
            // 从数据库获取企业信息
            // TODO: 根据实际企业表结构实现
            // List<Company> companies = companyMapper.listAll();

            // 示例：上报单个企业
            ProvincialUnitInfoDTO unitInfo = new ProvincialUnitInfoDTO();
            unitInfo.setUnitCode("53090200003");
            unitInfo.setUnitName("示例企业名称");
            unitInfo.setUnitType("危化品经营企业");
            unitInfo.setCreditCode("91530100MA6KXXXXXX");
            unitInfo.setLegalPerson("张三");
            unitInfo.setContactPerson("李四");
            unitInfo.setContactPhone("13800138000");
            unitInfo.setAddress("云南省昆明市某区某街道");
            unitInfo.setLongitude(102.712251);
            unitInfo.setLatitude(25.040609);
            unitInfo.setAreaCode("530902");
            unitInfo.setUnitStatus("在营");

            boolean success = provincialReportService.reportUnitInfo(unitInfo);
            if (success) {
                log.info("企业基础信息上报成功");
            } else {
                log.warn("企业基础信息上报失败");
            }
        } catch (Exception e) {
            log.error("企业基础信息上报异常: {}", e.getMessage(), e);
        }
    }

    /**
     * 定时上报预警信息（每5分钟执行一次）。
     */
    @Scheduled(fixedDelay = 300_000)
    public void reportWarning() {
        if (!enabled) {
            return;
        }

        log.debug("开始定时上报预警信息");

        try {
            // 从数据库获取未上报的预警信息
            // TODO: 根据实际告警表结构实现
            // List<Alarm> alarms = alarmMapper.listUnreported();

            // 示例：上报单个预警
            ProvincialWarningReportDTO warning = new ProvincialWarningReportDTO();
            warning.setWarningId("WARN_" + System.currentTimeMillis());
            warning.setUnitCode("53090200003");
            warning.setUnitName("示例企业名称");
            warning.setWarningType("温度超标");
            warning.setWarningLevel("高危");
            warning.setWarningTime(System.currentTimeMillis());
            warning.setWarningContent("1号仓库温度超过设定阈值");
            warning.setWarningStatus("未处理");
            warning.setLongitude(102.712251);
            warning.setLatitude(25.040609);
            warning.setDeviceCode("DEV001");
            warning.setDeviceName("温度传感器1号");
            warning.setStopType("停业");

            boolean success = provincialReportService.reportWarning(warning);
            if (success) {
                log.info("预警信息上报成功: {}", warning.getWarningId());
            } else {
                log.warn("预警信息上报失败: {}", warning.getWarningId());
            }
        } catch (Exception e) {
            log.error("预警信息上报异常: {}", e.getMessage(), e);
        }
    }
}
