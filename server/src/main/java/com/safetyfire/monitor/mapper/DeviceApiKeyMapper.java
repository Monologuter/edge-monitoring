package com.safetyfire.monitor.mapper;

import com.safetyfire.monitor.domain.entity.DeviceApiKeyEntity;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

/**
 * 设备上报鉴权（API Key + 签名）数据访问层。
 */
@Mapper
public interface DeviceApiKeyMapper {
    DeviceApiKeyEntity findByApiKey(@Param("apiKey") String apiKey);
}

