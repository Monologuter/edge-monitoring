package com.safetyfire.monitor.service;

import com.safetyfire.monitor.common.BizException;
import com.safetyfire.monitor.common.ErrorCode;
import com.safetyfire.monitor.common.PageResponse;
import com.safetyfire.monitor.domain.dto.AdminUserCreateRequest;
import com.safetyfire.monitor.domain.dto.AdminUserResetPasswordRequest;
import com.safetyfire.monitor.domain.dto.AdminUserUpdateRequest;
import com.safetyfire.monitor.domain.entity.UserEntity;
import com.safetyfire.monitor.domain.vo.AdminUserVO;
import com.safetyfire.monitor.mapper.UserCompanyScopeMapper;
import com.safetyfire.monitor.mapper.UserMapper;
import com.safetyfire.monitor.mapper.UserRoleMapper;
import com.safetyfire.monitor.security.DataScopeService;
import com.safetyfire.monitor.util.PasswordPolicy;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

/**
 * 管理后台：用户管理服务。
 */
@Service
public class AdminUserService {
    private final UserMapper userMapper;
    private final UserRoleMapper userRoleMapper;
    private final UserCompanyScopeMapper userCompanyScopeMapper;
    private final PasswordEncoder passwordEncoder;
    private final DataScopeService dataScopeService;

    public AdminUserService(UserMapper userMapper, UserRoleMapper userRoleMapper, UserCompanyScopeMapper userCompanyScopeMapper,
                            PasswordEncoder passwordEncoder, DataScopeService dataScopeService) {
        this.userMapper = userMapper;
        this.userRoleMapper = userRoleMapper;
        this.userCompanyScopeMapper = userCompanyScopeMapper;
        this.passwordEncoder = passwordEncoder;
        this.dataScopeService = dataScopeService;
    }

    public PageResponse<AdminUserVO> list(String keyword, int page, int pageSize) {
        int offset = (page - 1) * pageSize;
        List<UserEntity> users = userMapper.listUsers(keyword, offset, pageSize);
        long total = userMapper.countUsers(keyword);
        List<AdminUserVO> list = users.stream().map(u -> {
            List<String> roles = userMapper.listRolesByUserId(u.getId());
            List<String> companies = userCompanyScopeMapper.listCompanyCodesByUserId(u.getId());
            return new AdminUserVO(u.getId(), u.getUsername(), u.getDisplayName(), u.getEnabled(), roles, companies);
        }).toList();
        return new PageResponse<>(list, page, pageSize, total);
    }

    @Transactional
    public Long create(AdminUserCreateRequest req) {
        if (!PasswordPolicy.isStrong(req.password())) {
            throw new BizException(ErrorCode.PARAM_INVALID, "密码强度不足：需包含大小写字母与数字/符号");
        }
        if (userMapper.findByUsername(req.username()) != null) {
            throw new BizException(ErrorCode.STATE_CONFLICT, "用户名已存在");
        }
        boolean isAdminRole = req.roleKeys() != null && req.roleKeys().stream().anyMatch(s -> "ADMIN".equalsIgnoreCase(s));
        if (!isAdminRole && (req.companyCodes() == null || req.companyCodes().isEmpty())) {
            throw new BizException(ErrorCode.PARAM_INVALID, "非 ADMIN 用户必须配置数据范围（企业）");
        }

        UserEntity u = new UserEntity();
        u.setUsername(req.username());
        u.setDisplayName(req.displayName());
        u.setEnabled(req.enabled());
        u.setPasswordHash(passwordEncoder.encode(req.password()));
        userMapper.insert(u);

        setRoles(u.getId(), req.roleKeys());
        setCompanyScopes(u.getId(), req.companyCodes());
        dataScopeService.evictUserScope(u.getId());

        return u.getId();
    }

    @Transactional
    public void update(AdminUserUpdateRequest req) {
        UserEntity u = userMapper.findById(req.id());
        if (u == null) throw new BizException(ErrorCode.NOT_FOUND, "用户不存在");
        if ("admin".equals(u.getUsername())) {
            // admin 允许更新显示名/启用状态，但不允许移除其 ADMIN 角色（在 setRoles 中保护）
        }
        boolean isAdminRole = "admin".equals(u.getUsername()) || (req.roleKeys() != null && req.roleKeys().stream().anyMatch(s -> "ADMIN".equalsIgnoreCase(s)));
        if (!isAdminRole && (req.companyCodes() == null || req.companyCodes().isEmpty())) {
            throw new BizException(ErrorCode.PARAM_INVALID, "非 ADMIN 用户必须配置数据范围（企业）");
        }
        userMapper.updateBasic(u.getId(), req.displayName(), req.enabled());
        setRoles(u.getId(), req.roleKeys());
        setCompanyScopes(u.getId(), req.companyCodes());
        dataScopeService.evictUserScope(u.getId());
    }

    @Transactional
    public void delete(Long id) {
        UserEntity u = userMapper.findById(id);
        if (u == null) throw new BizException(ErrorCode.NOT_FOUND, "用户不存在");
        if ("admin".equals(u.getUsername())) {
            throw new BizException(ErrorCode.STATE_CONFLICT, "禁止删除系统管理员");
        }
        userRoleMapper.deleteByUserId(id);
        userCompanyScopeMapper.deleteByUserId(id);
        userMapper.deleteById(id);
        dataScopeService.evictUserScope(id);
    }

    @Transactional
    public void resetPassword(AdminUserResetPasswordRequest req) {
        if (!PasswordPolicy.isStrong(req.newPassword())) {
            throw new BizException(ErrorCode.PARAM_INVALID, "密码强度不足：需包含大小写字母与数字/符号");
        }
        UserEntity u = userMapper.findById(req.userId());
        if (u == null) throw new BizException(ErrorCode.NOT_FOUND, "用户不存在");
        userMapper.updatePassword(u.getId(), passwordEncoder.encode(req.newPassword()));
    }

    private void setRoles(Long userId, List<String> roleKeys) {
        userRoleMapper.deleteByUserId(userId);
        List<String> keys = roleKeys == null ? List.of() : roleKeys.stream().filter(s -> s != null && !s.isBlank()).distinct().toList();
        if (keys.isEmpty()) return;
        // admin 用户必须保留 ADMIN
        UserEntity u = userMapper.findById(userId);
        if (u != null && "admin".equals(u.getUsername()) && !keys.contains("ADMIN")) {
            keys = List.copyOf(keys);
            keys = new java.util.ArrayList<>(keys);
            keys.add("ADMIN");
        }
        for (String k : keys) {
            userRoleMapper.bindUserRole(userId, k);
        }
    }

    private void setCompanyScopes(Long userId, List<String> companyCodes) {
        userCompanyScopeMapper.deleteByUserId(userId);
        List<String> codes = companyCodes == null ? List.of() : companyCodes.stream().filter(s -> s != null && !s.isBlank()).distinct().toList();
        for (String c : codes) {
            userCompanyScopeMapper.insert(userId, c);
        }
    }
}
