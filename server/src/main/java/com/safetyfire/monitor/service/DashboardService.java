package com.safetyfire.monitor.service;

import com.safetyfire.monitor.domain.vo.DashboardVO;
import com.safetyfire.monitor.domain.entity.CompanyEntity;
import com.safetyfire.monitor.mapper.AlarmMapper;
import com.safetyfire.monitor.mapper.CarInoutRecordMapper;
import com.safetyfire.monitor.mapper.CompanyMapper;
import com.safetyfire.monitor.mapper.DeviceMapper;
import com.safetyfire.monitor.mapper.DeviceReadingHourMapper;
import com.safetyfire.monitor.mapper.PersonInoutRecordMapper;
import com.safetyfire.monitor.mapper.StoreroomMapper;
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
    private final CompanyMapper companyMapper;
    private final DeviceReadingHourMapper deviceReadingHourMapper;
    private final PersonInoutRecordMapper personInoutRecordMapper;
    private final CarInoutRecordMapper carInoutRecordMapper;
    private final StoreroomMapper storeroomMapper;

    public DashboardService(AlarmMapper alarmMapper, DeviceMapper deviceMapper, DataScopeService dataScopeService,
                            CompanyMapper companyMapper, DeviceReadingHourMapper deviceReadingHourMapper,
                            PersonInoutRecordMapper personInoutRecordMapper, CarInoutRecordMapper carInoutRecordMapper,
                            StoreroomMapper storeroomMapper) {
        this.alarmMapper = alarmMapper;
        this.deviceMapper = deviceMapper;
        this.dataScopeService = dataScopeService;
        this.companyMapper = companyMapper;
        this.deviceReadingHourMapper = deviceReadingHourMapper;
        this.personInoutRecordMapper = personInoutRecordMapper;
        this.carInoutRecordMapper = carInoutRecordMapper;
        this.storeroomMapper = storeroomMapper;
    }

    public DashboardVO overview() {
        List<String> scope = dataScopeService.currentCompanyCodesOrAll();
        long active = alarmMapper.countActive(scope);
        long totalDevices = deviceMapper.countAll(scope, null);
        long onlineDevices = deviceMapper.countOnline(scope);

        ZoneId zone = ZoneId.of("Asia/Shanghai");
        LocalDate today = LocalDate.now(zone);
        long startOfDay = today.atStartOfDay(zone).toInstant().toEpochMilli();
        long endOfDay = today.plusDays(1).atStartOfDay(zone).toInstant().toEpochMilli() - 1;
        long todayCount = alarmMapper.countToday(scope, startOfDay, endOfDay);

        long end = System.currentTimeMillis();
        long start = end - 24L * 60 * 60 * 1000;
        List<Long> trend = alarmMapper.countTrend24h(scope, start, end);

        CompanyEntity company = resolveCompany(scope);
        Double temp = deviceReadingHourMapper.findLatestValueByDeviceType(scope, 3);
        Double hum = deviceReadingHourMapper.findLatestValueByDeviceType(scope, 4);
        Double level = deviceReadingHourMapper.findLatestValueByDeviceType(scope, 5);

        long onsitePeople = personInoutRecordMapper.countLatestIn(scope);
        long onsiteCars = carInoutRecordMapper.countLatestIn(scope);
        long storeroomCount = storeroomMapper.count(null, null, scope);

        long todayInPeople = personInoutRecordMapper.countTodayIn(scope, startOfDay, endOfDay);
        long todayOutPeople = personInoutRecordMapper.countTodayOut(scope, startOfDay, endOfDay);
        long todayInCars = carInoutRecordMapper.countTodayIn(scope, startOfDay, endOfDay);
        long todayOutCars = carInoutRecordMapper.countTodayOut(scope, startOfDay, endOfDay);

        double onlineRate = totalDevices == 0 ? 0D : (onlineDevices * 100D / totalDevices);

        return new DashboardVO(
                company == null ? null : company.getCompanyName(),
                company == null ? null : company.getBusinessLicense(),
                temp,
                hum,
                level,
                onsitePeople,
                onsiteCars,
                storeroomCount,
                todayInPeople,
                todayOutPeople,
                todayInCars,
                todayOutCars,
                onlineDevices,
                totalDevices,
                onlineRate,
                active,
                todayCount,
                trend
        );
    }

    private CompanyEntity resolveCompany(List<String> scope) {
        if (scope != null && !scope.isEmpty()) {
            return companyMapper.findByCompanyCode(scope.get(0));
        }
        List<CompanyEntity> list = companyMapper.list(null, null, 0, 1);
        return list.isEmpty() ? null : list.get(0);
    }
}
