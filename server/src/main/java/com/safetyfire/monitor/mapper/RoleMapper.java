package com.safetyfire.monitor.mapper;

import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

/**
 * 角色数据访问层（主要用于初始化与鉴权拓展）。
 */
@Mapper
public interface RoleMapper {
    /**
     * 确保 ADMIN 角色存在。
     */
    void upsertAdminRole();

    void upsertRole(@Param("roleKey") String roleKey, @Param("roleName") String roleName);

    Long findIdByRoleKey(@Param("roleKey") String roleKey);
}
