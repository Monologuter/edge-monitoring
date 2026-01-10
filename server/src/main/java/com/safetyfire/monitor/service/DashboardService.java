package com.safetyfire.monitor.service;

import com.safetyfire.monitor.domain.vo.DashboardVO;
import com.safetyfire.monitor.mapper.AlarmMapper;
import com.safetyfire.monitor.mapper.DeviceMapper;
import com.safetyfire.monitor.security.DataScopeService;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.time.ZoneId;
import java.util.List;

/**
 * 首页概览服务：统计核心指标。
 */
@Service
public class DashboardService {
    private final AlarmMapper alarmMapper;
    private final DeviceMapper deviceMapper;
    private final DataScopeService dataScopeService;

    public DashboardService(AlarmMapper alarmMapper, DeviceMapper deviceMapper, DataScopeService dataScopeService) {
        this.alarmMapper = alarmMapper;
        this.deviceMapper = deviceMapper;
        this.dataScopeService = dataScopeService;
    }

    public DashboardVO overview() {
        List<String> scope = dataScopeService.currentCompanyCodesOrAll();
        long active = alarmMapper.countActive(scope);
        long totalDevices = deviceMapper.countAll(scope);
        long onlineDevices = deviceMapper.countOnline(scope);

        ZoneId zone = ZoneId.of("Asia/Shanghai");
        LocalDate today = LocalDate.now(zone);
        long startOfDay = today.atStartOfDay(zone).toInstant().toEpochMilli();
        long endOfDay = today.plusDays(1).atStartOfDay(zone).toInstant().toEpochMilli() - 1;
        long todayCount = alarmMapper.countToday(scope, startOfDay, endOfDay);

        long end = System.currentTimeMillis();
        long start = end - 24L * 60 * 60 * 1000;
        List<Long> trend = alarmMapper.countTrend24h(scope, start, end);

        return new DashboardVO(active, todayCount, onlineDevices, totalDevices, trend);
    }
}
