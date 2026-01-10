-- 设备上报安全增强（V3）

ALTER TABLE device_api_key
    ADD COLUMN api_secret VARCHAR(128) NULL COMMENT '设备上报密钥（用于签名；生产建议按设备分配并妥善保管）' AFTER api_key,
    ADD COLUMN allowed_ips VARCHAR(512) NULL COMMENT '允许的 IP 白名单（逗号分隔，空表示不限制）' AFTER enabled,
    ADD COLUMN rate_limit_per_minute INT NOT NULL DEFAULT 120 COMMENT '每分钟限流阈值（按 api_key+ip）' AFTER allowed_ips;

-- 为开发环境默认 Key 生成一个默认 secret（生产请替换）
UPDATE device_api_key
SET api_secret = COALESCE(api_secret, 'DEV_SECRET_001')
WHERE api_key = 'DEV_KEY_001';

