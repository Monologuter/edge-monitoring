-- 数据权限（V5）
-- 说明：以企业 company_code 为数据域，用户可被授予多个企业的数据访问范围。

CREATE TABLE IF NOT EXISTS user_company_scope
(
    id           BIGINT      NOT NULL AUTO_INCREMENT COMMENT '主键',
    user_id      BIGINT      NOT NULL COMMENT '用户ID',
    company_code VARCHAR(64) NOT NULL COMMENT '企业编码',
    created_at   DATETIME(3) NOT NULL COMMENT '创建时间',
    updated_at   DATETIME(3) NOT NULL COMMENT '更新时间',
    PRIMARY KEY (id),
    UNIQUE KEY uk_user_company (user_id, company_code),
    KEY idx_scope_user (user_id),
    KEY idx_scope_company (company_code)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4
  COMMENT = '用户-企业数据权限范围';

-- 为出入记录补充 company_code，便于数据权限过滤
ALTER TABLE person_inout_record
    ADD COLUMN company_code VARCHAR(64) NULL COMMENT '企业编码' AFTER id;

ALTER TABLE car_inout_record
    ADD COLUMN company_code VARCHAR(64) NULL COMMENT '企业编码' AFTER id;

-- 为告警补充 company_code，便于数据权限过滤
ALTER TABLE alarm_event
    ADD COLUMN company_code VARCHAR(64) NULL COMMENT '企业编码' AFTER id;

CREATE INDEX idx_person_inout_company ON person_inout_record (company_code);
CREATE INDEX idx_car_inout_company ON car_inout_record (company_code);
CREATE INDEX idx_alarm_company ON alarm_event (company_code);

