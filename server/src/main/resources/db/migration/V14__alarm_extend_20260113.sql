-- 告警字段补充（2026-01-13）

SET @exist := (
    SELECT COUNT(*)
    FROM INFORMATION_SCHEMA.COLUMNS
    WHERE TABLE_SCHEMA = DATABASE()
      AND TABLE_NAME = 'alarm_event'
      AND COLUMN_NAME = 'alarm_file'
);
SET @sql := IF(@exist = 0,
    'ALTER TABLE alarm_event ADD COLUMN alarm_file VARCHAR(255) NULL COMMENT ''告警图片/附件'' AFTER device_code',
    'SELECT ''Column alarm_file already exists'''
);
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;
