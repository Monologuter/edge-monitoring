package com.safetyfire.monitor.mapper;

import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

/**
 * 设备小时聚合数据访问层。
 */
@Mapper
public interface DeviceReadingHourMapper {
    void upsertSample(@Param("deviceCode") String deviceCode, @Param("hourStart") long hourStart, @Param("value") double value);

    Double findLatestValueByDeviceType(@Param("companyCodes") java.util.List<String> companyCodes,
                                       @Param("deviceType") int deviceType);
}
