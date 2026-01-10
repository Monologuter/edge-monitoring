-- AI 盒子（边缘计算单元）HTTP 推送接入（V10）
-- 目标：
-- 1) 兼容 /device/login、/device/heartBeat、/device/alarm/action、/device/image、/device/video
-- 2) 原始数据留痕（JSON/body + 上传媒体对应关系），便于排查
-- 3) 图片/视频存储到“可被 nginx/ftp 文件服务对外访问”的目录后，落库可追溯

CREATE TABLE IF NOT EXISTS ai_box_device
(
    id                    BIGINT       NOT NULL AUTO_INCREMENT COMMENT '主键',
    device_serial         VARCHAR(64)  NOT NULL COMMENT 'AI盒子序列号（唯一）',
    login_token           VARCHAR(128) NULL COMMENT '登录 token（兼容设备可选字段）',
    last_login_time_ms    BIGINT       NULL COMMENT '最近登录时间戳（毫秒）',
    last_heartbeat_time_ms BIGINT      NULL COMMENT '最近心跳时间戳（毫秒）',
    last_ip               VARCHAR(64)  NULL COMMENT '最近访问 IP',
    created_at            DATETIME(3)  NOT NULL COMMENT '创建时间',
    updated_at            DATETIME(3)  NOT NULL COMMENT '更新时间',
    PRIMARY KEY (id),
    UNIQUE KEY uk_ai_box_device_serial (device_serial),
    KEY idx_ai_box_device_heartbeat (last_heartbeat_time_ms)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4
  COMMENT = 'AI盒子设备表（HTTP推送接入）';

CREATE TABLE IF NOT EXISTS ai_box_alarm_action_ingest
(
    id            BIGINT       NOT NULL AUTO_INCREMENT COMMENT '主键',
    device_serial VARCHAR(64)  NULL COMMENT 'AI盒子序列号',
    alarm_type    VARCHAR(128) NULL COMMENT '告警/事件类型（可解析则填充）',
    alarm_time_ms BIGINT       NOT NULL COMMENT '告警时间戳（毫秒；解析失败则用接收时间）',
    raw_payload   TEXT         NOT NULL COMMENT '原始 JSON body（留痕）',
    created_at    DATETIME(3)  NOT NULL COMMENT '创建时间',
    updated_at    DATETIME(3)  NOT NULL COMMENT '更新时间',
    PRIMARY KEY (id),
    KEY idx_ai_box_alarm_action_device (device_serial),
    KEY idx_ai_box_alarm_action_time (alarm_time_ms)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4
  COMMENT = 'AI盒子行为告警推送（原始入库）';

CREATE TABLE IF NOT EXISTS ai_box_media
(
    id             BIGINT        NOT NULL AUTO_INCREMENT COMMENT '主键',
    device_serial  VARCHAR(64)   NULL COMMENT 'AI盒子序列号',
    alarm_action_id BIGINT       NULL COMMENT '关联行为告警入库记录ID（ai_box_alarm_action_ingest.id）',
    media_type     VARCHAR(16)   NOT NULL COMMENT '媒体类型：IMAGE/VIDEO',
    original_url   VARCHAR(1024) NULL COMMENT '设备上报的原始 URL（通常为局域网地址，不可公网访问）',
    file_object_id BIGINT        NULL COMMENT '落库文件ID（file_object.id）',
    public_url     VARCHAR(1024) NULL COMMENT '对外可访问 URL（如 http://47.120.48.64:9004/... ）',
    sha256         VARCHAR(64)   NULL COMMENT '文件 SHA256（便于去重/校验）',
    created_at     DATETIME(3)   NOT NULL COMMENT '创建时间',
    updated_at     DATETIME(3)   NOT NULL COMMENT '更新时间',
    PRIMARY KEY (id),
    KEY idx_ai_box_media_device (device_serial),
    KEY idx_ai_box_media_alarm (alarm_action_id),
    KEY idx_ai_box_media_type (media_type),
    KEY idx_ai_box_media_sha (sha256)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4
  COMMENT = 'AI盒子图片/录像上传记录';

