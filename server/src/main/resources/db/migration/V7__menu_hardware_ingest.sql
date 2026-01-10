-- 菜单补充：硬件数据（上报日志）（V7）
-- 说明：给值班员/管理员增加“硬件数据”入口，便于现场排查硬件上报。

INSERT INTO menu (menu_key, menu_name, path, icon, sort_no, enabled, created_at, updated_at)
VALUES ('hardware_ingest', '硬件数据', '/hardware/ingest', 'Connection', 120, 1, NOW(3), NOW(3))
ON DUPLICATE KEY UPDATE menu_name=VALUES(menu_name), path=VALUES(path), icon=VALUES(icon), sort_no=VALUES(sort_no), enabled=VALUES(enabled), updated_at=NOW(3);

-- 赋予 ADMIN 与 OPERATOR 可见
INSERT INTO role_menu (role_id, menu_id, created_at, updated_at)
SELECT r.id, m.id, NOW(3), NOW(3)
FROM role r
         INNER JOIN menu m ON m.menu_key = 'hardware_ingest'
WHERE r.role_key IN ('ADMIN', 'OPERATOR')
ON DUPLICATE KEY UPDATE updated_at = NOW(3);

