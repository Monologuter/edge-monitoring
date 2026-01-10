package com.safetyfire.monitor.config;

import com.safetyfire.monitor.domain.entity.UserEntity;
import com.safetyfire.monitor.mapper.RoleMapper;
import com.safetyfire.monitor.mapper.UserCompanyScopeMapper;
import com.safetyfire.monitor.mapper.UserRoleMapper;
import com.safetyfire.monitor.mapper.UserMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.CommandLineRunner;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Component;

/**
 * 初始化系统默认管理员账号（首次启动）。
 */
@Component
public class DataInitializer implements CommandLineRunner {
    private static final Logger log = LoggerFactory.getLogger(DataInitializer.class);

    private final UserMapper userMapper;
    private final RoleMapper roleMapper;
    private final UserRoleMapper userRoleMapper;
    private final UserCompanyScopeMapper userCompanyScopeMapper;
    private final PasswordEncoder passwordEncoder;

    public DataInitializer(UserMapper userMapper, RoleMapper roleMapper, UserRoleMapper userRoleMapper,
                           UserCompanyScopeMapper userCompanyScopeMapper, PasswordEncoder passwordEncoder) {
        this.userMapper = userMapper;
        this.roleMapper = roleMapper;
        this.userRoleMapper = userRoleMapper;
        this.userCompanyScopeMapper = userCompanyScopeMapper;
        this.passwordEncoder = passwordEncoder;
    }

    @Override
    public void run(String... args) {
        initAdmin();
        initTestUsers();
    }

    private void initAdmin() {
        String username = "admin";

        // 兜底确保 ADMIN 角色存在（数据库已预置，但为了可重复初始化，这里仍做一次保障）
        roleMapper.upsertAdminRole();

        UserEntity exists = userMapper.findByUsername(username);
        if (exists != null) {
            userRoleMapper.bindUserRole(exists.getId(), "ADMIN");
            return;
        }

        UserEntity admin = new UserEntity();
        admin.setUsername(username);
        admin.setDisplayName("系统管理员");
        admin.setEnabled(1);
        admin.setPasswordHash(passwordEncoder.encode("Admin@123456"));
        userMapper.insert(admin);

        userRoleMapper.bindUserRole(admin.getId(), "ADMIN");
        log.warn("已初始化默认管理员账号：用户名={} 密码=Admin@123456（请尽快登录后修改）", username);
    }

    /**
     * 初始化测试账号：用于验证“用户/角色/数据权限”。
     */
    private void initTestUsers() {
        // 兜底确保测试角色存在
        roleMapper.upsertRole("OPERATOR", "值班员");
        roleMapper.upsertRole("AUDITOR", "审计员");

        // 值班员：仅授权 C001
        initUserIfAbsent("operator1", "值班员-一号", "Operator@123456", "OPERATOR", "C001");
        // 审计员：仅授权 C002（主要用于查看审计/总览）
        initUserIfAbsent("auditor1", "审计员-一号", "Auditor@123456", "AUDITOR", "C002");
    }

    private void initUserIfAbsent(String username, String displayName, String password, String roleKey, String companyCode) {
        UserEntity exists = userMapper.findByUsername(username);
        if (exists != null) {
            userRoleMapper.bindUserRole(exists.getId(), roleKey);
            userCompanyScopeMapper.insert(exists.getId(), companyCode);
            return;
        }

        UserEntity u = new UserEntity();
        u.setUsername(username);
        u.setDisplayName(displayName);
        u.setEnabled(1);
        u.setPasswordHash(passwordEncoder.encode(password));
        userMapper.insert(u);

        userRoleMapper.bindUserRole(u.getId(), roleKey);
        userCompanyScopeMapper.insert(u.getId(), companyCode);
        log.warn("已初始化测试账号：用户名={} 密码={} 角色={} 数据范围={}", username, password, roleKey, companyCode);
    }
}
