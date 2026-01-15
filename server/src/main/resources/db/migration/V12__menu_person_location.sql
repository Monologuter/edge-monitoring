-- 人员定位菜单（2026-01-12）
INSERT INTO menu (id, parent_id, menu_key, menu_name, path, icon, sort_no, enabled, created_at, updated_at)
VALUES (12, NULL, 'personLocation', '人员定位', '/person-location', 'Monitor', 115, 1, NOW(3), NOW(3))
ON DUPLICATE KEY UPDATE menu_name=VALUES(menu_name), path=VALUES(path), icon=VALUES(icon), enabled=VALUES(enabled), updated_at=NOW(3);

INSERT INTO role_menu (role_id, menu_id, created_at, updated_at)
SELECT r.id, 12, NOW(3), NOW(3)
FROM role r
WHERE r.role_key = 'ADMIN'
ON DUPLICATE KEY UPDATE updated_at=NOW(3);
