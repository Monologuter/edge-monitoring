package com.safetyfire.monitor.mapper;

import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * 用户数据权限范围（企业维度）。
 */
@Mapper
public interface UserCompanyScopeMapper {
    List<String> listCompanyCodesByUserId(@Param("userId") Long userId);

    void insert(@Param("userId") Long userId, @Param("companyCode") String companyCode);

    int deleteByUserId(@Param("userId") Long userId);
}

