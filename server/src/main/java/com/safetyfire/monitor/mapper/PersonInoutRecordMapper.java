package com.safetyfire.monitor.mapper;

import com.safetyfire.monitor.domain.entity.PersonInoutRecordEntity;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * 人员出入记录访问层。
 */
@Mapper
public interface PersonInoutRecordMapper {
    void insert(PersonInoutRecordEntity e);

    List<PersonInoutRecordEntity> listByCompanyCodes(@Param("companyCodes") List<String> companyCodes,
                                                     @Param("offset") int offset, @Param("pageSize") int pageSize);

    long countByCompanyCodes(@Param("companyCodes") List<String> companyCodes);
}
