package com.safetyfire.monitor.mapper;

import com.safetyfire.monitor.domain.entity.HardwareIngestLogEntity;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * 硬件上报日志访问层。
 */
@Mapper
public interface HardwareIngestLogMapper {
    void insert(HardwareIngestLogEntity e);

    List<HardwareIngestLogEntity> list(
            @Param("scope") List<String> scope,
            @Param("channel") String channel,
            @Param("messageType") String messageType,
            @Param("apiKey") String apiKey,
            @Param("companyCode") String companyCode,
            @Param("ok") Integer ok,
            @Param("topicLike") String topicLike,
            @Param("offset") int offset,
            @Param("pageSize") int pageSize
    );

    long count(
            @Param("scope") List<String> scope,
            @Param("channel") String channel,
            @Param("messageType") String messageType,
            @Param("apiKey") String apiKey,
            @Param("companyCode") String companyCode,
            @Param("ok") Integer ok,
            @Param("topicLike") String topicLike
    );
}

