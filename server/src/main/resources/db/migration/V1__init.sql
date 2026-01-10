-- 安全监测预警系统初始化脚本（V1）
-- 注意：脚本中包含中文注释，便于后续审计与维护。

CREATE TABLE IF NOT EXISTS user_account
(
    id            BIGINT       NOT NULL AUTO_INCREMENT COMMENT '主键',
    username      VARCHAR(64)  NOT NULL COMMENT '用户名（唯一）',
    password_hash VARCHAR(255) NOT NULL COMMENT '密码哈希（BCrypt）',
    display_name  VARCHAR(64)  NOT NULL COMMENT '显示名称',
    enabled       TINYINT      NOT NULL DEFAULT 1 COMMENT '是否启用：1启用 0禁用',
    created_at    DATETIME(3)  NOT NULL COMMENT '创建时间',
    updated_at    DATETIME(3)  NOT NULL COMMENT '更新时间',
    PRIMARY KEY (id),
    UNIQUE KEY uk_user_username (username)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4
  COMMENT = '用户账户表';

CREATE TABLE IF NOT EXISTS role
(
    id         BIGINT      NOT NULL AUTO_INCREMENT COMMENT '主键',
    role_key   VARCHAR(64) NOT NULL COMMENT '角色标识，如 ADMIN/OPERATOR',
    role_name  VARCHAR(64) NOT NULL COMMENT '角色名称',
    created_at DATETIME(3) NOT NULL COMMENT '创建时间',
    updated_at DATETIME(3) NOT NULL COMMENT '更新时间',
    PRIMARY KEY (id),
    UNIQUE KEY uk_role_key (role_key)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4
  COMMENT = '角色表';

CREATE TABLE IF NOT EXISTS user_role
(
    id         BIGINT     NOT NULL AUTO_INCREMENT COMMENT '主键',
    user_id    BIGINT     NOT NULL COMMENT '用户ID',
    role_id    BIGINT     NOT NULL COMMENT '角色ID',
    created_at DATETIME(3) NOT NULL COMMENT '创建时间',
    updated_at DATETIME(3) NOT NULL COMMENT '更新时间',
    PRIMARY KEY (id),
    UNIQUE KEY uk_user_role (user_id, role_id),
    KEY idx_user_role_user (user_id),
    KEY idx_user_role_role (role_id)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4
  COMMENT = '用户-角色关联表';

-- 预置角色（可按需扩展权限体系）
INSERT INTO role (id, role_key, role_name, created_at, updated_at)
VALUES (1, 'ADMIN', '系统管理员', NOW(3), NOW(3))
ON DUPLICATE KEY UPDATE role_name=VALUES(role_name), updated_at=NOW(3);

-- 将 admin 用户绑定为 ADMIN 角色（admin 用户由程序首次启动初始化）
INSERT INTO user_role (user_id, role_id, created_at, updated_at)
SELECT ua.id, 1, NOW(3), NOW(3)
FROM user_account ua
WHERE ua.username = 'admin'
ON DUPLICATE KEY UPDATE updated_at = NOW(3);

CREATE TABLE IF NOT EXISTS device
(
    id            BIGINT       NOT NULL AUTO_INCREMENT COMMENT '主键',
    device_code   VARCHAR(64)  NOT NULL COMMENT '设备编码（唯一）',
    device_name   VARCHAR(128) NOT NULL COMMENT '设备名称',
    device_type   INT          NOT NULL COMMENT '设备类型：1视频 2红外 3温度 4湿度 5液位',
    unit          VARCHAR(16)  NULL COMMENT '单位，如 ℃/%/mm',
    lower_limit   DOUBLE       NULL COMMENT '下限阈值（触发告警）',
    upper_limit   DOUBLE       NULL COMMENT '上限阈值（触发告警）',
    location_name VARCHAR(128) NULL COMMENT '位置描述（仓库/库房/点位）',
    online_status TINYINT      NOT NULL DEFAULT 0 COMMENT '在线状态：1在线 0离线',
    created_at    DATETIME(3)  NOT NULL COMMENT '创建时间',
    updated_at    DATETIME(3)  NOT NULL COMMENT '更新时间',
    PRIMARY KEY (id),
    UNIQUE KEY uk_device_code (device_code)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4
  COMMENT = '设备表';

CREATE TABLE IF NOT EXISTS device_api_key
(
    id         BIGINT      NOT NULL AUTO_INCREMENT COMMENT '主键',
    api_key    VARCHAR(128) NOT NULL COMMENT '设备上报 API Key（明文存储便于现场配置；生产建议改为哈希）',
    enabled    TINYINT     NOT NULL DEFAULT 1 COMMENT '是否启用：1启用 0禁用',
    remark     VARCHAR(255) NULL COMMENT '备注',
    created_at DATETIME(3) NOT NULL COMMENT '创建时间',
    updated_at DATETIME(3) NOT NULL COMMENT '更新时间',
    PRIMARY KEY (id),
    UNIQUE KEY uk_device_api_key (api_key)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4
  COMMENT = '设备上报鉴权 Key 表';

-- 预置一个开发环境 API Key：DEV_KEY_001（请生产环境替换）
INSERT INTO device_api_key (id, api_key, enabled, remark, created_at, updated_at)
VALUES (1, 'DEV_KEY_001', 1, '开发环境默认 Key，请生产替换', NOW(3), NOW(3))
ON DUPLICATE KEY UPDATE enabled=VALUES(enabled), updated_at=NOW(3);

CREATE TABLE IF NOT EXISTS alarm_event
(
    id          BIGINT       NOT NULL AUTO_INCREMENT COMMENT '主键',
    alarm_type  VARCHAR(64)  NOT NULL COMMENT '告警类型（超员/堵塞通道/超高超量/非法入侵/遮挡偏移/阈值超限等）',
    alarm_status VARCHAR(32) NOT NULL COMMENT '告警状态：ACTIVE/CLEARED/ARCHIVED',
    risk_level  VARCHAR(16)  NOT NULL COMMENT '风险等级：LOW/MEDIUM/HIGH',
    device_code VARCHAR(64)  NULL COMMENT '关联设备编码（如摄像头/温湿度）',
    warning_time BIGINT      NOT NULL COMMENT '告警时间戳（毫秒）',
    clear_time  BIGINT       NULL COMMENT '消警时间戳（毫秒）',
    handler     VARCHAR(64)  NULL COMMENT '处理人（用户名）',
    remark      VARCHAR(255) NULL COMMENT '备注（处理说明/附加信息）',
    created_at  DATETIME(3)  NOT NULL COMMENT '创建时间',
    updated_at  DATETIME(3)  NOT NULL COMMENT '更新时间',
    PRIMARY KEY (id),
    KEY idx_alarm_status (alarm_status),
    KEY idx_alarm_time (warning_time),
    KEY idx_alarm_device (device_code)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4
  COMMENT = '告警事件表（告警闭环）';

-- 演示数据：设备与阈值（可删除）
INSERT INTO device (id, device_code, device_name, device_type, unit, lower_limit, upper_limit, location_name, online_status, created_at, updated_at)
VALUES (1, 'TEMP_001', '库房温度传感器-01', 3, '℃', 0, 35, '1号库房', 1, NOW(3), NOW(3))
ON DUPLICATE KEY UPDATE updated_at=NOW(3);

