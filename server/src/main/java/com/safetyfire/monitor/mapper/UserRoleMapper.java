package com.safetyfire.monitor.mapper;

import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

/**
 * 用户角色关联访问层。
 */
@Mapper
public interface UserRoleMapper {
    /**
     * 绑定用户角色（按 role_key 绑定，避免依赖固定 role_id）。
     */
    void bindUserRole(@Param("userId") Long userId, @Param("roleKey") String roleKey);

    int deleteByUserId(@Param("userId") Long userId);
}
