package com.safetyfire.monitor.mapper;

import com.safetyfire.monitor.domain.entity.DeviceEntity;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * 设备数据访问层。
 */
@Mapper
public interface DeviceMapper {
    DeviceEntity findByDeviceCode(@Param("deviceCode") String deviceCode);

    DeviceEntity findById(@Param("id") Long id);

    void insert(DeviceEntity e);

    int update(DeviceEntity e);

    int deleteById(@Param("id") Long id);

    List<DeviceEntity> list(@Param("companyCodes") List<String> companyCodes,
                            @Param("offset") int offset, @Param("pageSize") int pageSize,
                            @Param("deviceType") Integer deviceType);

    long countAll(@Param("companyCodes") List<String> companyCodes, @Param("deviceType") Integer deviceType);

    default long countAll(@Param("companyCodes") List<String> companyCodes) {
        return countAll(companyCodes, null);
    }

    long countOnline(@Param("companyCodes") List<String> companyCodes);

    void updateHeartbeat(@Param("deviceCode") String deviceCode, @Param("heartbeatTime") long heartbeatTime);

    int markOfflineBefore(@Param("thresholdTime") long thresholdTime);
}
