package com.safetyfire.monitor.mapper;

import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * 权限数据访问层。
 */
@Mapper
public interface PermissionMapper {
    /**
     * 查询用户拥有的权限标识列表（按角色聚合）。
     */
    List<String> listPermKeysByUserId(@Param("userId") Long userId);
}

