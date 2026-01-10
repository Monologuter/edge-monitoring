package com.safetyfire.monitor.mapper;

import com.safetyfire.monitor.domain.entity.AiBoxAlarmActionIngestEntity;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

/**
 * AI盒子行为告警推送入库访问层。
 */
@Mapper
public interface AiBoxAlarmActionIngestMapper {
    void insert(AiBoxAlarmActionIngestEntity e);

    AiBoxAlarmActionIngestEntity findById(@Param("id") Long id);
}

