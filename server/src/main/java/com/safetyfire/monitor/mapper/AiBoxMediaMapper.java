package com.safetyfire.monitor.mapper;

import com.safetyfire.monitor.domain.entity.AiBoxMediaEntity;
import org.apache.ibatis.annotations.Mapper;

/**
 * AI盒子媒体上传记录访问层。
 */
@Mapper
public interface AiBoxMediaMapper {
    void insert(AiBoxMediaEntity e);
}

