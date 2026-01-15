-- 预警推送记录与温湿度液位菜单（2026-01-13）
INSERT INTO menu (id, parent_id, menu_key, menu_name, path, icon, sort_no, enabled, created_at, updated_at)
VALUES (14, NULL, 'linkageRecords', '预警推送记录', '/linkage-records', 'DataLine', 25, 1, NOW(3), NOW(3)),
       (15, NULL, 'temperature', '温度信息', '/devices/temperature', 'Cpu', 71, 1, NOW(3), NOW(3)),
       (16, NULL, 'humidity', '湿度信息', '/devices/humidity', 'Cpu', 72, 1, NOW(3), NOW(3)),
       (17, NULL, 'level', '液位信息', '/devices/level', 'Cpu', 73, 1, NOW(3), NOW(3))
ON DUPLICATE KEY UPDATE menu_name=VALUES(menu_name), path=VALUES(path), icon=VALUES(icon), enabled=VALUES(enabled), updated_at=NOW(3);

INSERT INTO role_menu (role_id, menu_id, created_at, updated_at)
SELECT r.id, m.id, NOW(3), NOW(3)
FROM role r
         INNER JOIN menu m ON m.id IN (14, 15, 16, 17)
WHERE r.role_key = 'ADMIN'
ON DUPLICATE KEY UPDATE updated_at=NOW(3);
