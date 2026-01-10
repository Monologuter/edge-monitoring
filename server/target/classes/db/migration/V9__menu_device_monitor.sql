-- 菜单补充：设备实时监控（V9）

INSERT INTO menu (menu_key, menu_name, path, icon, sort_no, enabled, created_at, updated_at)
VALUES ('device_monitor', '设备监控', '/devices/monitor', 'DataLine', 85, 1, NOW(3), NOW(3))
ON DUPLICATE KEY UPDATE menu_name=VALUES(menu_name), path=VALUES(path), icon=VALUES(icon), sort_no=VALUES(sort_no), enabled=VALUES(enabled), updated_at=NOW(3);

-- 赋予 ADMIN 与 OPERATOR 可见
INSERT INTO role_menu (role_id, menu_id, created_at, updated_at)
SELECT r.id, m.id, NOW(3), NOW(3)
FROM role r
         INNER JOIN menu m ON m.menu_key = 'device_monitor'
WHERE r.role_key IN ('ADMIN', 'OPERATOR')
ON DUPLICATE KEY UPDATE updated_at = NOW(3);
