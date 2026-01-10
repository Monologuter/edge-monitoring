package com.safetyfire.monitor.mapper;

import com.safetyfire.monitor.domain.entity.AiBoxDeviceEntity;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

/**
 * AI盒子设备访问层。
 */
@Mapper
public interface AiBoxDeviceMapper {
    AiBoxDeviceEntity findByDeviceSerial(@Param("deviceSerial") String deviceSerial);

    void insert(AiBoxDeviceEntity e);

    void updateLogin(@Param("deviceSerial") String deviceSerial,
                     @Param("loginToken") String loginToken,
                     @Param("loginTimeMs") Long loginTimeMs,
                     @Param("lastIp") String lastIp);

    void updateHeartbeat(@Param("deviceSerial") String deviceSerial,
                         @Param("heartbeatTimeMs") Long heartbeatTimeMs,
                         @Param("lastIp") String lastIp);
}

