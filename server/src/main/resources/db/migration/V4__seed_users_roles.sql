-- 初始化测试数据（V4）
-- 说明：插入企业、角色、权限、菜单等基础数据，便于本地快速体验“用户/角色/数据权限”。

-- ========== 企业测试数据 ==========
INSERT INTO company (company_code, company_name, business_license, created_at, updated_at)
VALUES ('C001', '测试企业-一号', '91320000TEST0001', NOW(3), NOW(3)),
       ('C002', '测试企业-二号', '91320000TEST0002', NOW(3), NOW(3))
ON DUPLICATE KEY UPDATE company_name=VALUES(company_name), updated_at=NOW(3);

-- ========== 角色 ==========
INSERT INTO role (role_key, role_name, created_at, updated_at)
VALUES ('OPERATOR', '值班员', NOW(3), NOW(3)),
       ('AUDITOR', '审计员', NOW(3), NOW(3))
ON DUPLICATE KEY UPDATE role_name=VALUES(role_name), updated_at=NOW(3);

-- ========== 权限（补充 admin 管理权限） ==========
INSERT INTO permission (perm_key, perm_name, created_at, updated_at)
VALUES ('admin:manage', '系统管理（用户/角色/数据权限）', NOW(3), NOW(3))
ON DUPLICATE KEY UPDATE perm_name=VALUES(perm_name), updated_at=NOW(3);

-- ADMIN 赋予 admin:manage（其余权限在 V2 已全量赋予）
INSERT INTO role_permission (role_id, perm_id, created_at, updated_at)
SELECT r.id, p.id, NOW(3), NOW(3)
FROM role r
         INNER JOIN permission p ON p.perm_key = 'admin:manage'
WHERE r.role_key = 'ADMIN'
ON DUPLICATE KEY UPDATE updated_at = NOW(3);

-- OPERATOR：基础业务权限（示例）
INSERT INTO role_permission (role_id, perm_id, created_at, updated_at)
SELECT r.id, p.id, NOW(3), NOW(3)
FROM role r
         INNER JOIN permission p ON p.perm_key IN ('dashboard:read', 'alarm:manage', 'device:manage', 'company:manage', 'person:manage', 'car:manage', 'store:manage', 'camera:manage')
WHERE r.role_key = 'OPERATOR'
ON DUPLICATE KEY UPDATE updated_at = NOW(3);

-- AUDITOR：只读审计权限（示例）
INSERT INTO role_permission (role_id, perm_id, created_at, updated_at)
SELECT r.id, p.id, NOW(3), NOW(3)
FROM role r
         INNER JOIN permission p ON p.perm_key IN ('audit:read', 'dashboard:read')
WHERE r.role_key = 'AUDITOR'
ON DUPLICATE KEY UPDATE updated_at = NOW(3);

-- ========== 菜单（补充系统管理菜单） ==========
INSERT INTO menu (menu_key, menu_name, path, icon, sort_no, enabled, created_at, updated_at)
VALUES ('admin_users', '用户管理', '/admin/users', 'Setting', 200, 1, NOW(3), NOW(3)),
       ('admin_roles', '角色管理', '/admin/roles', 'Setting', 210, 1, NOW(3), NOW(3))
ON DUPLICATE KEY UPDATE menu_name=VALUES(menu_name), path=VALUES(path), icon=VALUES(icon), sort_no=VALUES(sort_no), enabled=VALUES(enabled), updated_at=NOW(3);

INSERT INTO role_menu (role_id, menu_id, created_at, updated_at)
SELECT r.id, m.id, NOW(3), NOW(3)
FROM role r
         INNER JOIN menu m ON m.menu_key IN ('admin_users', 'admin_roles')
WHERE r.role_key = 'ADMIN'
ON DUPLICATE KEY UPDATE updated_at = NOW(3);

-- OPERATOR 菜单：业务操作
INSERT INTO role_menu (role_id, menu_id, created_at, updated_at)
SELECT r.id, m.id, NOW(3), NOW(3)
FROM role r
         INNER JOIN menu m ON m.menu_key IN ('dashboard', 'alarms', 'company', 'people', 'cars', 'stores', 'devices', 'cameras', 'screen')
WHERE r.role_key = 'OPERATOR'
ON DUPLICATE KEY UPDATE updated_at = NOW(3);

-- AUDITOR 菜单：审计与总览
INSERT INTO role_menu (role_id, menu_id, created_at, updated_at)
SELECT r.id, m.id, NOW(3), NOW(3)
FROM role r
         INNER JOIN menu m ON m.menu_key IN ('dashboard', 'audit')
WHERE r.role_key = 'AUDITOR'
ON DUPLICATE KEY UPDATE updated_at = NOW(3);
