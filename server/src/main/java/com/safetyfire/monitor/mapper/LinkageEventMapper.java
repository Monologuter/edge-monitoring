package com.safetyfire.monitor.mapper;

import com.safetyfire.monitor.domain.entity.LinkageEventEntity;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * 联动事件访问层。
 */
@Mapper
public interface LinkageEventMapper {
    void insert(LinkageEventEntity e);

    List<LinkageEventEntity> list(@Param("offset") int offset, @Param("pageSize") int pageSize, @Param("status") String status);

    long count(@Param("status") String status);

    List<LinkageEventEntity> listPending(@Param("limit") int limit);

    void markSent(@Param("id") Long id);

    void markFailed(@Param("id") Long id, @Param("lastError") String lastError);
}

