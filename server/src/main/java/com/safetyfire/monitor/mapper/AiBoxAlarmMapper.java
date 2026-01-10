package com.safetyfire.monitor.mapper;

import com.safetyfire.monitor.domain.entity.AiBoxAlarmEntity;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * AI盒子告警记录访问层。
 */
@Mapper
public interface AiBoxAlarmMapper {
    void insert(AiBoxAlarmEntity e);

    List<AiBoxAlarmEntity> listByCompanyCodes(@Param("companyCodes") List<String> companyCodes,
                                              @Param("offset") int offset, @Param("pageSize") int pageSize,
                                              @Param("alarmType") String alarmType);

    long countByCompanyCodes(@Param("companyCodes") List<String> companyCodes,
                             @Param("alarmType") String alarmType);
}
