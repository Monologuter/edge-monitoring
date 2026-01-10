package com.safetyfire.monitor.mapper;

import com.safetyfire.monitor.domain.entity.ProvincialWarningFeedbackEntity;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * 省厅预警反馈访问层。
 */
@Mapper
public interface ProvincialWarningFeedbackMapper {
    void insert(ProvincialWarningFeedbackEntity e);

    List<ProvincialWarningFeedbackEntity> list(@Param("offset") int offset, @Param("pageSize") int pageSize);

    long count();
}

