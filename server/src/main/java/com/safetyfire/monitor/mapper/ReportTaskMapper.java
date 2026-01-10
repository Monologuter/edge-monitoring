package com.safetyfire.monitor.mapper;

import com.safetyfire.monitor.domain.entity.ReportTaskEntity;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * 对接/上报任务访问层。
 */
@Mapper
public interface ReportTaskMapper {
    void insertIgnore(ReportTaskEntity e);

    List<ReportTaskEntity> listDueTasks(@Param("taskType") String taskType, @Param("now") long now, @Param("limit") int limit);

    int markRunning(@Param("id") Long id);

    void markSuccess(@Param("id") Long id);

    void markFailed(@Param("id") Long id, @Param("lastError") String lastError, @Param("retryCount") int retryCount, @Param("nextRetryTime") long nextRetryTime);
}

