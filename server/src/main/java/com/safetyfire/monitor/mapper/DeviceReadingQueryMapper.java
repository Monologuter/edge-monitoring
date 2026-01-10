package com.safetyfire.monitor.mapper;

import com.safetyfire.monitor.domain.vo.DeviceReadingHourVO;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * 设备采集数据查询（统计/趋势）。
 */
@Mapper
public interface DeviceReadingQueryMapper {
    List<DeviceReadingHourVO> listHourly(@Param("deviceCode") String deviceCode, @Param("start") long start, @Param("end") long end);
}

