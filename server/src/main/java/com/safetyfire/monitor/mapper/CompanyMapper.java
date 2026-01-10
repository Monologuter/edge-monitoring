package com.safetyfire.monitor.mapper;

import com.safetyfire.monitor.domain.entity.CompanyEntity;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * 企业信息访问层。
 */
@Mapper
public interface CompanyMapper {
    CompanyEntity findById(@Param("id") Long id);

    CompanyEntity findByCompanyCode(@Param("companyCode") String companyCode);

    void insert(CompanyEntity e);

    int update(CompanyEntity e);

    int deleteById(@Param("id") Long id);

    List<CompanyEntity> list(@Param("keyword") String keyword, @Param("companyCodes") List<String> companyCodes,
                             @Param("offset") int offset, @Param("pageSize") int pageSize);

    long count(@Param("keyword") String keyword, @Param("companyCodes") List<String> companyCodes);
}
