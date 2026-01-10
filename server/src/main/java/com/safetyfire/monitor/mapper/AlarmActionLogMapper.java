package com.safetyfire.monitor.mapper;

import com.safetyfire.monitor.domain.entity.AlarmActionLogEntity;
import org.apache.ibatis.annotations.Mapper;

/**
 * 告警动作日志访问层。
 */
@Mapper
public interface AlarmActionLogMapper {
    void insert(AlarmActionLogEntity e);
}

