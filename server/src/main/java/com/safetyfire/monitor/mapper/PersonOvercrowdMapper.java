package com.safetyfire.monitor.mapper;

import com.safetyfire.monitor.domain.entity.PersonOvercrowdEntity;
import org.apache.ibatis.annotations.Mapper;

/**
 * 人员超员记录访问层。
 */
@Mapper
public interface PersonOvercrowdMapper {
    void insert(PersonOvercrowdEntity e);
}
