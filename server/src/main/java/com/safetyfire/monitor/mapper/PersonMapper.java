package com.safetyfire.monitor.mapper;

import com.safetyfire.monitor.domain.entity.PersonEntity;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * 人员档案访问层。
 */
@Mapper
public interface PersonMapper {
    PersonEntity findById(@Param("id") Long id);

    PersonEntity findByIdcard(@Param("idcard") String idcard);

    void insert(PersonEntity e);

    int update(PersonEntity e);

    int deleteById(@Param("id") Long id);

    List<PersonEntity> list(@Param("keyword") String keyword, @Param("companyCodes") List<String> companyCodes,
                            @Param("offset") int offset, @Param("pageSize") int pageSize);

    long count(@Param("keyword") String keyword, @Param("companyCodes") List<String> companyCodes);
}
