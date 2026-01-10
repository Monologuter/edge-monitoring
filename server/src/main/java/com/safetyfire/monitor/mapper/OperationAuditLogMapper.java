package com.safetyfire.monitor.mapper;

import com.safetyfire.monitor.domain.entity.OperationAuditLogEntity;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * 操作审计日志访问层。
 */
@Mapper
public interface OperationAuditLogMapper {
    void insert(OperationAuditLogEntity e);

    List<OperationAuditLogEntity> list(@Param("offset") int offset, @Param("pageSize") int pageSize);

    long count();
}

