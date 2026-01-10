package com.safetyfire.monitor.mapper;

import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * 管理后台：角色管理访问层。
 */
@Mapper
public interface AdminRoleMapper {
    List<RoleRow> list(@Param("keyword") String keyword, @Param("offset") int offset, @Param("pageSize") int pageSize);

    long count(@Param("keyword") String keyword);

    RoleRow findById(@Param("id") Long id);

    RoleRow findByRoleKey(@Param("roleKey") String roleKey);

    void insert(RoleRow r);

    int update(RoleRow r);

    int deleteById(@Param("id") Long id);

    List<String> listPermissionKeys(@Param("roleId") Long roleId);

    List<String> listMenuKeys(@Param("roleId") Long roleId);

    int deleteRolePermissions(@Param("roleId") Long roleId);

    int insertRolePermissions(@Param("roleId") Long roleId, @Param("permKeys") List<String> permKeys);

    int deleteRoleMenus(@Param("roleId") Long roleId);

    int insertRoleMenus(@Param("roleId") Long roleId, @Param("menuKeys") List<String> menuKeys);

    /**
     * 内部行对象（role 表字段）。
     */
    class RoleRow {
        private Long id;
        private String roleKey;
        private String roleName;

        public Long getId() {
            return id;
        }

        public void setId(Long id) {
            this.id = id;
        }

        public String getRoleKey() {
            return roleKey;
        }

        public void setRoleKey(String roleKey) {
            this.roleKey = roleKey;
        }

        public String getRoleName() {
            return roleName;
        }

        public void setRoleName(String roleName) {
            this.roleName = roleName;
        }
    }
}

