-- AI盒子告警表和人员超员表（V8）

-- AI盒子告警表（通道堵塞、超高超重、摄像头遮挡偏移等）
CREATE TABLE IF NOT EXISTS ai_box_alarm
(
    id            BIGINT       NOT NULL AUTO_INCREMENT COMMENT '主键',
    company_code  VARCHAR(64)  NULL COMMENT '企业编码',
    alarm_type    VARCHAR(64)  NOT NULL COMMENT '告警类型：通道堵塞/超高超重/摄像头遮挡偏移',
    alarm_status  VARCHAR(32)  NOT NULL COMMENT '告警状态：ACTIVE/CLEARED',
    warning_time  BIGINT       NOT NULL COMMENT '告警时间戳（毫秒）',
    camera_code   VARCHAR(64)  NULL COMMENT '摄像头编码',
    area_code     VARCHAR(64)  NULL COMMENT '区域编码',
    height        DOUBLE       NULL COMMENT '物体高度（米）',
    weight        DOUBLE       NULL COMMENT '物体重量（吨）',
    description   VARCHAR(512) NULL COMMENT '告警描述',
    image_file_id BIGINT       NULL COMMENT '告警图片附件ID',
    created_at    DATETIME(3)  NOT NULL COMMENT '创建时间',
    updated_at    DATETIME(3)  NOT NULL COMMENT '更新时间',
    PRIMARY KEY (id),
    KEY idx_ai_box_alarm_company (company_code),
    KEY idx_ai_box_alarm_type (alarm_type),
    KEY idx_ai_box_alarm_time (warning_time),
    KEY idx_ai_box_alarm_camera (camera_code)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4
  COMMENT = 'AI盒子告警表';

-- 人员超员记录表
CREATE TABLE IF NOT EXISTS person_overcrowd
(
    id            BIGINT      NOT NULL AUTO_INCREMENT COMMENT '主键',
    company_code  VARCHAR(64) NULL COMMENT '企业编码',
    area_code     VARCHAR(64) NOT NULL COMMENT '区域编码',
    area_name     VARCHAR(128) NOT NULL COMMENT '区域名称',
    current_count INT         NOT NULL COMMENT '当前人数',
    max_count     INT         NOT NULL COMMENT '最大人数（阈值）',
    warning_time  BIGINT      NOT NULL COMMENT '告警时间戳（毫秒）',
    created_at    DATETIME(3) NOT NULL COMMENT '创建时间',
    updated_at    DATETIME(3) NOT NULL COMMENT '更新时间',
    PRIMARY KEY (id),
    KEY idx_overcrowd_company (company_code),
    KEY idx_overcrowd_area (area_code),
    KEY idx_overcrowd_time (warning_time)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4
  COMMENT = '人员超员记录表';

-- 为alarm_event表添加company_code字段（如果还没有）
SET @exist := (SELECT COUNT(*) FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'alarm_event' AND COLUMN_NAME = 'company_code');
SET @sql := IF(@exist = 0, 'ALTER TABLE alarm_event ADD COLUMN company_code VARCHAR(64) NULL COMMENT ''企业编码'' AFTER id', 'SELECT ''Column already exists''');
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;

SET @exist := (SELECT COUNT(*) FROM INFORMATION_SCHEMA.STATISTICS WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'alarm_event' AND INDEX_NAME = 'idx_alarm_company');
SET @sql := IF(@exist = 0, 'CREATE INDEX idx_alarm_company ON alarm_event(company_code)', 'SELECT ''Index already exists''');
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;
