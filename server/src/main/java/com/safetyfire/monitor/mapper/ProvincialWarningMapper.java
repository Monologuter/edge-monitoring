package com.safetyfire.monitor.mapper;

import com.safetyfire.monitor.domain.entity.ProvincialWarningEntity;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * 省厅预警访问层。
 */
@Mapper
public interface ProvincialWarningMapper {
    void upsert(ProvincialWarningEntity e);

    List<ProvincialWarningEntity> list(@Param("offset") int offset, @Param("pageSize") int pageSize);

    long count();
}

