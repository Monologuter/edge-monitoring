package com.safetyfire.monitor.service;

import com.safetyfire.monitor.common.BizException;
import com.safetyfire.monitor.common.ErrorCode;
import com.safetyfire.monitor.common.PageResponse;
import com.safetyfire.monitor.domain.dto.AdminRoleUpsertRequest;
import com.safetyfire.monitor.domain.vo.AdminRoleVO;
import com.safetyfire.monitor.domain.vo.MenuVO;
import com.safetyfire.monitor.mapper.AdminMetaMapper;
import com.safetyfire.monitor.mapper.AdminRoleMapper;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

/**
 * 管理后台：角色管理服务。
 */
@Service
public class AdminRoleService {
    private final AdminRoleMapper adminRoleMapper;
    private final AdminMetaMapper adminMetaMapper;

    public AdminRoleService(AdminRoleMapper adminRoleMapper, AdminMetaMapper adminMetaMapper) {
        this.adminRoleMapper = adminRoleMapper;
        this.adminMetaMapper = adminMetaMapper;
    }

    public PageResponse<AdminRoleVO> list(String keyword, int page, int pageSize) {
        int offset = (page - 1) * pageSize;
        List<AdminRoleMapper.RoleRow> rows = adminRoleMapper.list(keyword, offset, pageSize);
        long total = adminRoleMapper.count(keyword);
        List<AdminRoleVO> list = rows.stream().map(r -> new AdminRoleVO(
                r.getId(),
                r.getRoleKey(),
                r.getRoleName(),
                adminRoleMapper.listPermissionKeys(r.getId()),
                adminRoleMapper.listMenuKeys(r.getId())
        )).toList();
        return new PageResponse<>(list, page, pageSize, total);
    }

    public List<String> listAllPermissionKeys() {
        return adminMetaMapper.listAllPermissionKeys();
    }

    public List<MenuVO> listAllMenus() {
        return adminMetaMapper.listAllMenus();
    }

    @Transactional
    public Long upsert(AdminRoleUpsertRequest req) {
        String roleKey = req.roleKey().trim();
        if ("ADMIN".equals(roleKey)) {
            throw new BizException(ErrorCode.STATE_CONFLICT, "禁止修改 ADMIN 角色");
        }

        if (req.id() == null) {
            if (adminRoleMapper.findByRoleKey(roleKey) != null) {
                throw new BizException(ErrorCode.STATE_CONFLICT, "角色标识已存在");
            }
            AdminRoleMapper.RoleRow r = new AdminRoleMapper.RoleRow();
            r.setRoleKey(roleKey);
            r.setRoleName(req.roleName());
            adminRoleMapper.insert(r);
            updateBindings(r.getId(), req.permissionKeys(), req.menuKeys());
            return r.getId();
        }

        AdminRoleMapper.RoleRow existing = adminRoleMapper.findById(req.id());
        if (existing == null) throw new BizException(ErrorCode.NOT_FOUND, "角色不存在");
        AdminRoleMapper.RoleRow byKey = adminRoleMapper.findByRoleKey(roleKey);
        if (byKey != null && !byKey.getId().equals(req.id())) {
            throw new BizException(ErrorCode.STATE_CONFLICT, "角色标识已存在");
        }

        existing.setRoleKey(roleKey);
        existing.setRoleName(req.roleName());
        adminRoleMapper.update(existing);
        updateBindings(existing.getId(), req.permissionKeys(), req.menuKeys());
        return existing.getId();
    }

    @Transactional
    public void delete(Long id) {
        AdminRoleMapper.RoleRow existing = adminRoleMapper.findById(id);
        if (existing == null) throw new BizException(ErrorCode.NOT_FOUND, "角色不存在");
        if ("ADMIN".equals(existing.getRoleKey())) {
            throw new BizException(ErrorCode.STATE_CONFLICT, "禁止删除 ADMIN 角色");
        }
        adminRoleMapper.deleteRoleMenus(id);
        adminRoleMapper.deleteRolePermissions(id);
        adminRoleMapper.deleteById(id);
    }

    private void updateBindings(Long roleId, List<String> permissionKeys, List<String> menuKeys) {
        adminRoleMapper.deleteRolePermissions(roleId);
        List<String> perms = permissionKeys == null ? List.of() : permissionKeys.stream().filter(s -> s != null && !s.isBlank()).distinct().toList();
        if (!perms.isEmpty()) {
            adminRoleMapper.insertRolePermissions(roleId, perms);
        }
        adminRoleMapper.deleteRoleMenus(roleId);
        List<String> menus = menuKeys == null ? List.of() : menuKeys.stream().filter(s -> s != null && !s.isBlank()).distinct().toList();
        if (!menus.isEmpty()) {
            adminRoleMapper.insertRoleMenus(roleId, menus);
        }
    }
}

