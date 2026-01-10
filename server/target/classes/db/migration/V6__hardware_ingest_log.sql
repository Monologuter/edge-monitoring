-- 硬件上报日志（V6）
-- 目标：让后台可以“看到硬件传递过来的原始数据”，并记录解析成功/失败原因，便于现场排查。

CREATE TABLE IF NOT EXISTS hardware_ingest_log
(
    id             BIGINT       NOT NULL AUTO_INCREMENT COMMENT '主键',
    ingest_channel VARCHAR(16)  NOT NULL COMMENT '接入通道：MQTT/HTTP',
    topic          VARCHAR(255) NULL COMMENT 'MQTT topic / HTTP path',
    message_type   VARCHAR(64)  NOT NULL COMMENT '消息类型：device-reading/alarm/person-inout/car-inout/server-heartbeat',
    api_key        VARCHAR(64)  NULL COMMENT '设备上报 apiKey（X-Device-Api-Key / MQTT envelope.apiKey）',
    company_code   VARCHAR(64)  NULL COMMENT '解析关联企业编码（用于数据权限过滤；无法解析则为空）',
    payload        TEXT         NOT NULL COMMENT '原始 payload（MQTT 为 envelope JSON；HTTP 为 body JSON）',
    parsed_ok      TINYINT      NOT NULL DEFAULT 1 COMMENT '是否解析成功：1成功 0失败',
    error_message  VARCHAR(512) NULL COMMENT '失败原因（截断）',
    receive_time_ms BIGINT      NOT NULL COMMENT '接收时间戳（毫秒）',
    created_at     DATETIME(3)  NOT NULL COMMENT '创建时间',
    updated_at     DATETIME(3)  NOT NULL COMMENT '更新时间',
    PRIMARY KEY (id),
    KEY idx_ingest_time_ms (receive_time_ms),
    KEY idx_ingest_type (message_type),
    KEY idx_ingest_company (company_code),
    KEY idx_ingest_api (api_key)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4
  COMMENT = '硬件上报日志（原始数据留痕）';

