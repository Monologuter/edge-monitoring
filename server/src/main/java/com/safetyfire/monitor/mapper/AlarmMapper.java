package com.safetyfire.monitor.mapper;

import com.safetyfire.monitor.domain.entity.AlarmEntity;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * 告警数据访问层。
 */
@Mapper
public interface AlarmMapper {
    void insert(AlarmEntity alarm);

    AlarmEntity findById(@Param("id") Long id);

    void updateHandle(@Param("id") Long id, @Param("handler") String handler, @Param("remark") String remark);

    void updateClear(@Param("id") Long id, @Param("clearTime") Long clearTime, @Param("handler") String handler, @Param("remark") String remark);

    void updateArchive(@Param("id") Long id, @Param("archivedTime") Long archivedTime, @Param("handler") String handler, @Param("remark") String remark);

    long countActive(@Param("companyCodes") List<String> companyCodes);

    long countToday(@Param("companyCodes") List<String> companyCodes, @Param("startOfDay") long startOfDay, @Param("endOfDay") long endOfDay);

    List<Long> countTrend24h(@Param("companyCodes") List<String> companyCodes, @Param("start") long start, @Param("end") long end);

    List<AlarmEntity> list(@Param("companyCodes") List<String> companyCodes, @Param("offset") int offset, @Param("pageSize") int pageSize, @Param("status") String status);

    long count(@Param("companyCodes") List<String> companyCodes, @Param("status") String status);
}
