package com.safetyfire.monitor.mapper;

import com.safetyfire.monitor.domain.entity.DeviceApiKeyEntity;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * 设备上报鉴权（API Key + 签名）数据访问层。
 */
@Mapper
public interface DeviceApiKeyMapper {
    DeviceApiKeyEntity findByApiKey(@Param("apiKey") String apiKey);

    List<DeviceApiKeyEntity> findAll();

    DeviceApiKeyEntity findById(@Param("id") Long id);

    void insert(DeviceApiKeyEntity entity);

    void update(DeviceApiKeyEntity entity);

    void deleteById(@Param("id") Long id);
}

