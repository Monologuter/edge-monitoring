-- 安全监测预警系统业务表（V2）
-- 说明：为满足 FR-01 ~ FR-12，引入企业档案、人员车辆、仓库库房、视频、上报/对接、省厅预警、联动、文件与审计、RBAC 权限等核心表。

-- ========== RBAC 权限体系 ==========

CREATE TABLE IF NOT EXISTS permission
(
    id          BIGINT      NOT NULL AUTO_INCREMENT COMMENT '主键',
    perm_key    VARCHAR(128) NOT NULL COMMENT '权限标识，如 company:read / alarm:handle',
    perm_name   VARCHAR(128) NOT NULL COMMENT '权限名称',
    created_at  DATETIME(3) NOT NULL COMMENT '创建时间',
    updated_at  DATETIME(3) NOT NULL COMMENT '更新时间',
    PRIMARY KEY (id),
    UNIQUE KEY uk_permission_key (perm_key)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4
  COMMENT = '权限表';

CREATE TABLE IF NOT EXISTS role_permission
(
    id         BIGINT     NOT NULL AUTO_INCREMENT COMMENT '主键',
    role_id    BIGINT     NOT NULL COMMENT '角色ID',
    perm_id    BIGINT     NOT NULL COMMENT '权限ID',
    created_at DATETIME(3) NOT NULL COMMENT '创建时间',
    updated_at DATETIME(3) NOT NULL COMMENT '更新时间',
    PRIMARY KEY (id),
    UNIQUE KEY uk_role_perm (role_id, perm_id),
    KEY idx_role_perm_role (role_id),
    KEY idx_role_perm_perm (perm_id)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4
  COMMENT = '角色-权限关联表';

CREATE TABLE IF NOT EXISTS menu
(
    id          BIGINT       NOT NULL AUTO_INCREMENT COMMENT '主键',
    parent_id   BIGINT       NULL COMMENT '父菜单ID',
    menu_key    VARCHAR(128) NOT NULL COMMENT '菜单标识（前端路由/资源标识）',
    menu_name   VARCHAR(128) NOT NULL COMMENT '菜单名称',
    path        VARCHAR(255) NULL COMMENT '路由路径',
    icon        VARCHAR(64)  NULL COMMENT '图标',
    sort_no     INT          NOT NULL DEFAULT 0 COMMENT '排序',
    enabled     TINYINT      NOT NULL DEFAULT 1 COMMENT '是否启用：1启用 0禁用',
    created_at  DATETIME(3)  NOT NULL COMMENT '创建时间',
    updated_at  DATETIME(3)  NOT NULL COMMENT '更新时间',
    PRIMARY KEY (id),
    UNIQUE KEY uk_menu_key (menu_key),
    KEY idx_menu_parent (parent_id)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4
  COMMENT = '菜单表';

CREATE TABLE IF NOT EXISTS role_menu
(
    id         BIGINT     NOT NULL AUTO_INCREMENT COMMENT '主键',
    role_id    BIGINT     NOT NULL COMMENT '角色ID',
    menu_id    BIGINT     NOT NULL COMMENT '菜单ID',
    created_at DATETIME(3) NOT NULL COMMENT '创建时间',
    updated_at DATETIME(3) NOT NULL COMMENT '更新时间',
    PRIMARY KEY (id),
    UNIQUE KEY uk_role_menu (role_id, menu_id),
    KEY idx_role_menu_role (role_id),
    KEY idx_role_menu_menu (menu_id)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4
  COMMENT = '角色-菜单关联表';

-- ========== 文件与媒体 ==========

CREATE TABLE IF NOT EXISTS file_object
(
    id           BIGINT       NOT NULL AUTO_INCREMENT COMMENT '主键',
    biz_type     VARCHAR(64)  NOT NULL COMMENT '业务类型：license/alarm/person_inout/car_inout/other',
    original_name VARCHAR(255) NOT NULL COMMENT '原始文件名',
    content_type VARCHAR(128) NOT NULL COMMENT 'Content-Type',
    size_bytes   BIGINT       NOT NULL COMMENT '文件大小（字节）',
    sha256       VARCHAR(64)  NOT NULL COMMENT 'SHA256（去重与校验）',
    storage_type VARCHAR(32)  NOT NULL COMMENT '存储类型：LOCAL（后续可扩展 OSS）',
    storage_path VARCHAR(512) NOT NULL COMMENT '存储路径（相对/绝对）',
    created_by   VARCHAR(64)  NULL COMMENT '上传人（用户名）',
    created_at   DATETIME(3)  NOT NULL COMMENT '创建时间',
    updated_at   DATETIME(3)  NOT NULL COMMENT '更新时间',
    PRIMARY KEY (id),
    UNIQUE KEY uk_file_sha (sha256),
    KEY idx_file_biz (biz_type)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4
  COMMENT = '文件对象表';

-- ========== 企业档案（FR-01） ==========

CREATE TABLE IF NOT EXISTS company
(
    id                          BIGINT       NOT NULL AUTO_INCREMENT COMMENT '主键',
    company_code                VARCHAR(64)  NOT NULL COMMENT '企业编码（对接省厅/本系统唯一标识）',
    company_name                VARCHAR(255) NOT NULL COMMENT '企业名称',
    business_license            VARCHAR(64)  NULL COMMENT '营业执照号',
    business_license_file_id    BIGINT       NULL COMMENT '营业执照附件（file_object.id）',
    business_license_start      DATE         NULL COMMENT '营业执照开始日期',
    business_license_end        DATE         NULL COMMENT '营业执照结束日期',
    business_license_scope      VARCHAR(512) NULL COMMENT '经营范围',
    business_license_issuing_authority VARCHAR(128) NULL COMMENT '发证机关',
    address                     VARCHAR(255) NULL COMMENT '企业地址',
    register_address            VARCHAR(255) NULL COMMENT '注册地址',
    company_status              VARCHAR(32)  NULL COMMENT '企业状态（在业/停产等）',
    dosage                      DOUBLE       NULL COMMENT '核定药量（示例字段）',
    reservoir_area              DOUBLE       NULL COMMENT '库区面积（示例字段）',
    created_at                  DATETIME(3)  NOT NULL COMMENT '创建时间',
    updated_at                  DATETIME(3)  NOT NULL COMMENT '更新时间',
    PRIMARY KEY (id),
    UNIQUE KEY uk_company_code (company_code),
    KEY idx_company_name (company_name)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4
  COMMENT = '企业信息表';

-- ========== 仓库/库房（FR-04） ==========

CREATE TABLE IF NOT EXISTS store
(
    id          BIGINT       NOT NULL AUTO_INCREMENT COMMENT '主键',
    company_code VARCHAR(64) NOT NULL COMMENT '企业编码',
    store_num   VARCHAR(64)  NOT NULL COMMENT '仓库编码（唯一）',
    store_name  VARCHAR(255) NOT NULL COMMENT '仓库名称',
    area        DOUBLE       NULL COMMENT '面积',
    danger_level VARCHAR(8)  NULL COMMENT '危险等级：01/02/03',
    quota_dosage DOUBLE      NULL COMMENT '核定药量',
    quota_people INT         NULL COMMENT '核定人数',
    created_at  DATETIME(3)  NOT NULL COMMENT '创建时间',
    updated_at  DATETIME(3)  NOT NULL COMMENT '更新时间',
    PRIMARY KEY (id),
    UNIQUE KEY uk_store_num (store_num),
    KEY idx_store_company (company_code)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4
  COMMENT = '仓库表';

CREATE TABLE IF NOT EXISTS storeroom
(
    id           BIGINT       NOT NULL AUTO_INCREMENT COMMENT '主键',
    store_num    VARCHAR(64)  NOT NULL COMMENT '仓库编码',
    storeroom_num VARCHAR(64) NOT NULL COMMENT '库房编码（唯一）',
    storeroom_name VARCHAR(255) NOT NULL COMMENT '库房名称',
    area         DOUBLE       NULL COMMENT '面积',
    danger_level VARCHAR(8)   NULL COMMENT '危险等级：01/02/03',
    quota_dosage DOUBLE       NULL COMMENT '核定药量',
    quota_people INT          NULL COMMENT '核定人数',
    created_at   DATETIME(3)  NOT NULL COMMENT '创建时间',
    updated_at   DATETIME(3)  NOT NULL COMMENT '更新时间',
    PRIMARY KEY (id),
    UNIQUE KEY uk_storeroom_num (storeroom_num),
    KEY idx_storeroom_store (store_num)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4
  COMMENT = '库房表';

-- ========== 人员/车辆（FR-02/FR-03） ==========

CREATE TABLE IF NOT EXISTS person
(
    id           BIGINT       NOT NULL AUTO_INCREMENT COMMENT '主键',
    company_code VARCHAR(64)  NOT NULL COMMENT '企业编码',
    person_name  VARCHAR(64)  NOT NULL COMMENT '姓名',
    idcard       VARCHAR(32)  NOT NULL COMMENT '身份证号（敏感字段，展示需脱敏）',
    person_type  VARCHAR(32)  NOT NULL COMMENT '人员类型：法定代表人/负责人/安全管理/特种作业/其他',
    is_certified TINYINT      NOT NULL DEFAULT 0 COMMENT '是否持证：1是 0否',
    phone        VARCHAR(32)  NULL COMMENT '联系方式',
    created_at   DATETIME(3)  NOT NULL COMMENT '创建时间',
    updated_at   DATETIME(3)  NOT NULL COMMENT '更新时间',
    PRIMARY KEY (id),
    UNIQUE KEY uk_person_idcard (idcard),
    KEY idx_person_company (company_code)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4
  COMMENT = '人员档案表';

CREATE TABLE IF NOT EXISTS car
(
    id                 BIGINT       NOT NULL AUTO_INCREMENT COMMENT '主键',
    company_code        VARCHAR(64)  NOT NULL COMMENT '企业编码',
    license_plate_number VARCHAR(32) NOT NULL COMMENT '车牌号（唯一）',
    car_type            VARCHAR(32)  NULL COMMENT '车辆类型：危化/其他',
    created_at          DATETIME(3)  NOT NULL COMMENT '创建时间',
    updated_at          DATETIME(3)  NOT NULL COMMENT '更新时间',
    PRIMARY KEY (id),
    UNIQUE KEY uk_car_plate (license_plate_number),
    KEY idx_car_company (company_code)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4
  COMMENT = '车辆档案表';

CREATE TABLE IF NOT EXISTS person_inout_record
(
    id          BIGINT       NOT NULL AUTO_INCREMENT COMMENT '主键',
    idcard      VARCHAR(32)  NOT NULL COMMENT '身份证号',
    person_name VARCHAR(64)  NOT NULL COMMENT '姓名',
    person_type VARCHAR(32)  NOT NULL COMMENT '人员类型：员工/访客/其他',
    in_out_state VARCHAR(8)  NOT NULL COMMENT '进出状态：IN/OUT',
    in_out_time BIGINT       NOT NULL COMMENT '进出时间戳（毫秒）',
    image_file_id BIGINT     NULL COMMENT '抓拍图片（file_object.id）',
    created_at  DATETIME(3)  NOT NULL COMMENT '创建时间',
    updated_at  DATETIME(3)  NOT NULL COMMENT '更新时间',
    PRIMARY KEY (id),
    KEY idx_person_inout_time (in_out_time),
    KEY idx_person_inout_idcard (idcard)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4
  COMMENT = '人员出入记录表';

CREATE TABLE IF NOT EXISTS car_inout_record
(
    id                  BIGINT       NOT NULL AUTO_INCREMENT COMMENT '主键',
    license_plate_number VARCHAR(32) NOT NULL COMMENT '车牌号',
    car_type            VARCHAR(32)  NULL COMMENT '车辆类型：危化/其他',
    in_out_state        VARCHAR(8)   NOT NULL COMMENT '进出状态：IN/OUT',
    in_out_time         BIGINT       NOT NULL COMMENT '进出时间戳（毫秒）',
    image_file_id       BIGINT       NULL COMMENT '抓拍图片（file_object.id）',
    created_at          DATETIME(3)  NOT NULL COMMENT '创建时间',
    updated_at          DATETIME(3)  NOT NULL COMMENT '更新时间',
    PRIMARY KEY (id),
    KEY idx_car_inout_time (in_out_time),
    KEY idx_car_inout_plate (license_plate_number)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4
  COMMENT = '车辆出入记录表';

-- ========== 设备与采集（FR-05/FR-08） ==========

ALTER TABLE device
    ADD COLUMN company_code VARCHAR(64) NULL COMMENT '企业编码' AFTER id,
    ADD COLUMN store_num VARCHAR(64) NULL COMMENT '仓库编码' AFTER location_name,
    ADD COLUMN storeroom_num VARCHAR(64) NULL COMMENT '库房编码' AFTER store_num,
    ADD COLUMN last_heartbeat_time BIGINT NULL COMMENT '最近心跳时间戳（毫秒）' AFTER online_status;

CREATE TABLE IF NOT EXISTS device_reading_hour
(
    id          BIGINT      NOT NULL AUTO_INCREMENT COMMENT '主键',
    device_code VARCHAR(64) NOT NULL COMMENT '设备编码',
    hour_start  BIGINT      NOT NULL COMMENT '小时开始时间戳（毫秒）',
    avg_value   DOUBLE      NOT NULL COMMENT '小时平均值',
    min_value   DOUBLE      NOT NULL COMMENT '小时最小值',
    max_value   DOUBLE      NOT NULL COMMENT '小时最大值',
    samples     INT         NOT NULL COMMENT '样本数',
    created_at  DATETIME(3) NOT NULL COMMENT '创建时间',
    updated_at  DATETIME(3) NOT NULL COMMENT '更新时间',
    PRIMARY KEY (id),
    UNIQUE KEY uk_device_hour (device_code, hour_start),
    KEY idx_device_hour_time (hour_start)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4
  COMMENT = '设备小时聚合数据（每小时一条）';

-- ========== 视频接入（FR-09） ==========

CREATE TABLE IF NOT EXISTS camera
(
    id           BIGINT       NOT NULL AUTO_INCREMENT COMMENT '主键',
    company_code VARCHAR(64)  NULL COMMENT '企业编码',
    camera_code  VARCHAR(64)  NOT NULL COMMENT '摄像头编码（唯一）',
    camera_name  VARCHAR(255) NOT NULL COMMENT '摄像头名称',
    stream_url   VARCHAR(512) NOT NULL COMMENT '流地址（RTSP/HTTP-FLV/HLS 等）',
    location_name VARCHAR(255) NULL COMMENT '位置描述',
    store_num    VARCHAR(64)  NULL COMMENT '仓库编码',
    storeroom_num VARCHAR(64) NULL COMMENT '库房编码',
    enabled      TINYINT      NOT NULL DEFAULT 1 COMMENT '是否启用：1启用 0禁用',
    created_at   DATETIME(3)  NOT NULL COMMENT '创建时间',
    updated_at   DATETIME(3)  NOT NULL COMMENT '更新时间',
    PRIMARY KEY (id),
    UNIQUE KEY uk_camera_code (camera_code),
    KEY idx_camera_company (company_code)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4
  COMMENT = '视频接入表';

-- ========== 告警闭环增强（FR-06/FR-12） ==========

ALTER TABLE alarm_event
    ADD COLUMN workflow_status VARCHAR(32) NOT NULL DEFAULT 'NEW' COMMENT '闭环状态：NEW/ACKED/PROCESSING/CLEARED/ARCHIVED' AFTER alarm_status,
    ADD COLUMN archived_time BIGINT NULL COMMENT '归档时间戳（毫秒）' AFTER clear_time;

CREATE TABLE IF NOT EXISTS alarm_action_log
(
    id          BIGINT      NOT NULL AUTO_INCREMENT COMMENT '主键',
    alarm_id    BIGINT      NOT NULL COMMENT '告警ID',
    action      VARCHAR(32) NOT NULL COMMENT '动作：CREATE/ACK/HANDLE/CLEAR/ARCHIVE',
    operator    VARCHAR(64) NULL COMMENT '操作人',
    remark      VARCHAR(255) NULL COMMENT '说明',
    action_time BIGINT      NOT NULL COMMENT '动作时间戳（毫秒）',
    created_at  DATETIME(3) NOT NULL COMMENT '创建时间',
    updated_at  DATETIME(3) NOT NULL COMMENT '更新时间',
    PRIMARY KEY (id),
    KEY idx_alarm_action_alarm (alarm_id),
    KEY idx_alarm_action_time (action_time)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4
  COMMENT = '告警动作日志（闭环轨迹）';

-- ========== 企业服务器与心跳（需求数据对象） ==========

CREATE TABLE IF NOT EXISTS server_machine
(
    id           BIGINT       NOT NULL AUTO_INCREMENT COMMENT '主键',
    company_code VARCHAR(64)  NOT NULL COMMENT '企业编码',
    computer_name VARCHAR(128) NOT NULL COMMENT '服务器名称',
    ip           VARCHAR(64)  NOT NULL COMMENT 'IP',
    original_id  VARCHAR(64)  NULL COMMENT '对接原始ID',
    created_at   DATETIME(3)  NOT NULL COMMENT '创建时间',
    updated_at   DATETIME(3)  NOT NULL COMMENT '更新时间',
    PRIMARY KEY (id),
    UNIQUE KEY uk_server_ip (ip),
    KEY idx_server_company (company_code)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4
  COMMENT = '企业服务器信息表';

CREATE TABLE IF NOT EXISTS server_heartbeat
(
    id           BIGINT      NOT NULL AUTO_INCREMENT COMMENT '主键',
    ip           VARCHAR(64) NOT NULL COMMENT '服务器IP',
    heartbeat_time BIGINT     NOT NULL COMMENT '心跳时间戳（毫秒）',
    created_at   DATETIME(3) NOT NULL COMMENT '创建时间',
    updated_at   DATETIME(3) NOT NULL COMMENT '更新时间',
    PRIMARY KEY (id),
    KEY idx_heartbeat_ip (ip),
    KEY idx_heartbeat_time (heartbeat_time)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4
  COMMENT = '服务器心跳表';

-- ========== 省厅预警对接（FR-07/FR-10） ==========

CREATE TABLE IF NOT EXISTS provincial_warning
(
    id           BIGINT       NOT NULL AUTO_INCREMENT COMMENT '主键',
    external_id  VARCHAR(128) NOT NULL COMMENT '省厅预警ID/唯一标识',
    company_code VARCHAR(64)  NULL COMMENT '企业编码（如可获取）',
    risk_level   VARCHAR(32)  NULL COMMENT '风险等级',
    push_type    VARCHAR(32)  NULL COMMENT '推送类型',
    warning_time BIGINT       NULL COMMENT '预警时间戳（毫秒）',
    raw_json     JSON         NOT NULL COMMENT '原始报文',
    created_at   DATETIME(3)  NOT NULL COMMENT '创建时间',
    updated_at   DATETIME(3)  NOT NULL COMMENT '更新时间',
    PRIMARY KEY (id),
    UNIQUE KEY uk_warning_external (external_id),
    KEY idx_warning_time (warning_time)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4
  COMMENT = '省厅预警入库表';

CREATE TABLE IF NOT EXISTS provincial_warning_feedback
(
    id           BIGINT       NOT NULL AUTO_INCREMENT COMMENT '主键',
    external_id  VARCHAR(128) NOT NULL COMMENT '省厅预警ID/唯一标识',
    feedback     VARCHAR(512) NOT NULL COMMENT '反馈内容',
    status       VARCHAR(32)  NOT NULL DEFAULT 'PENDING' COMMENT '状态：PENDING/SENT/FAILED',
    last_error   VARCHAR(512) NULL COMMENT '最后一次错误',
    created_at   DATETIME(3)  NOT NULL COMMENT '创建时间',
    updated_at   DATETIME(3)  NOT NULL COMMENT '更新时间',
    PRIMARY KEY (id),
    KEY idx_feedback_external (external_id),
    KEY idx_feedback_status (status)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4
  COMMENT = '省厅预警反馈表';

CREATE TABLE IF NOT EXISTS report_task
(
    id          BIGINT       NOT NULL AUTO_INCREMENT COMMENT '主键',
    task_type   VARCHAR(64)  NOT NULL COMMENT '任务类型：PROVINCIAL_REPORT/PROVINCIAL_FEEDBACK 等',
    biz_key     VARCHAR(128) NOT NULL COMMENT '业务键（去重）',
    payload_json JSON        NOT NULL COMMENT '任务载荷',
    status      VARCHAR(32)  NOT NULL DEFAULT 'PENDING' COMMENT '状态：PENDING/RUNNING/SUCCESS/FAILED',
    retry_count INT          NOT NULL DEFAULT 0 COMMENT '重试次数',
    next_retry_time BIGINT   NULL COMMENT '下次重试时间戳（毫秒）',
    last_error  VARCHAR(512) NULL COMMENT '最后一次错误',
    created_at  DATETIME(3)  NOT NULL COMMENT '创建时间',
    updated_at  DATETIME(3)  NOT NULL COMMENT '更新时间',
    PRIMARY KEY (id),
    UNIQUE KEY uk_report_task (task_type, biz_key),
    KEY idx_report_status (status)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4
  COMMENT = '对接/上报任务表（重试与追踪）';

-- ========== 预警联动（FR-11） ==========

CREATE TABLE IF NOT EXISTS linkage_event
(
    id          BIGINT       NOT NULL AUTO_INCREMENT COMMENT '主键',
    alarm_id    BIGINT       NULL COMMENT '关联告警ID',
    linkage_type VARCHAR(64) NOT NULL COMMENT '联动类型：IP_TTS/EMAIL/SMS/WEBHOOK 等',
    target      VARCHAR(255) NOT NULL COMMENT '联动目标（IP/URL/号码等）',
    payload     VARCHAR(512) NULL COMMENT '联动内容（如 TTS 文本）',
    status      VARCHAR(32)  NOT NULL DEFAULT 'PENDING' COMMENT '状态：PENDING/SENT/FAILED',
    last_error  VARCHAR(512) NULL COMMENT '最后一次错误',
    created_at  DATETIME(3)  NOT NULL COMMENT '创建时间',
    updated_at  DATETIME(3)  NOT NULL COMMENT '更新时间',
    PRIMARY KEY (id),
    KEY idx_linkage_alarm (alarm_id),
    KEY idx_linkage_status (status)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4
  COMMENT = '联动事件表';

-- ========== 操作审计 ==========

CREATE TABLE IF NOT EXISTS operation_audit_log
(
    id          BIGINT       NOT NULL AUTO_INCREMENT COMMENT '主键',
    request_id  VARCHAR(64)  NULL COMMENT '请求ID',
    username    VARCHAR(64)  NULL COMMENT '操作人',
    uri         VARCHAR(255) NOT NULL COMMENT '请求URI',
    method      VARCHAR(16)  NOT NULL COMMENT 'HTTP 方法',
    ip          VARCHAR(64)  NULL COMMENT '客户端IP',
    action      VARCHAR(64)  NULL COMMENT '业务动作（审计点）',
    request_json JSON        NULL COMMENT '请求摘要（脱敏后）',
    response_code INT        NULL COMMENT '响应 code',
    cost_ms     BIGINT       NULL COMMENT '耗时（ms）',
    created_at  DATETIME(3)  NOT NULL COMMENT '创建时间',
    updated_at  DATETIME(3)  NOT NULL COMMENT '更新时间',
    PRIMARY KEY (id),
    KEY idx_audit_user (username),
    KEY idx_audit_time (created_at)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4
  COMMENT = '操作审计日志表';

-- 预置权限（最小集合，后续按模块扩展）
INSERT INTO permission (perm_key, perm_name, created_at, updated_at)
VALUES ('dashboard:read', '查看总览', NOW(3), NOW(3)),
       ('company:manage', '企业管理', NOW(3), NOW(3)),
       ('person:manage', '人员管理', NOW(3), NOW(3)),
       ('car:manage', '车辆管理', NOW(3), NOW(3)),
       ('store:manage', '仓库库房管理', NOW(3), NOW(3)),
       ('device:manage', '设备管理', NOW(3), NOW(3)),
       ('alarm:manage', '告警管理', NOW(3), NOW(3)),
       ('camera:manage', '视频接入管理', NOW(3), NOW(3)),
       ('provincial:manage', '省厅对接管理', NOW(3), NOW(3)),
       ('audit:read', '审计日志查看', NOW(3), NOW(3))
ON DUPLICATE KEY UPDATE perm_name=VALUES(perm_name), updated_at=NOW(3);

-- 将 ADMIN 角色赋予全部权限
INSERT INTO role_permission (role_id, perm_id, created_at, updated_at)
SELECT r.id, p.id, NOW(3), NOW(3)
FROM role r
         CROSS JOIN permission p
WHERE r.role_key = 'ADMIN'
ON DUPLICATE KEY UPDATE updated_at = NOW(3);

-- 预置菜单
INSERT INTO menu (id, parent_id, menu_key, menu_name, path, icon, sort_no, enabled, created_at, updated_at)
VALUES (1, NULL, 'dashboard', '总览', '/dashboard', 'Monitor', 10, 1, NOW(3), NOW(3)),
       (2, NULL, 'alarms', '告警中心', '/alarms', 'Bell', 20, 1, NOW(3), NOW(3)),
       (3, NULL, 'company', '企业档案', '/company', 'OfficeBuilding', 30, 1, NOW(3), NOW(3)),
       (4, NULL, 'people', '人员管理', '/people', 'User', 40, 1, NOW(3), NOW(3)),
       (5, NULL, 'cars', '车辆管理', '/cars', 'Van', 50, 1, NOW(3), NOW(3)),
       (6, NULL, 'stores', '仓库库房', '/stores', 'Box', 60, 1, NOW(3), NOW(3)),
       (7, NULL, 'devices', '设备管理', '/devices', 'Cpu', 70, 1, NOW(3), NOW(3)),
       (8, NULL, 'cameras', '视频接入', '/cameras', 'VideoCamera', 80, 1, NOW(3), NOW(3)),
       (9, NULL, 'provincial', '省厅对接', '/provincial', 'Connection', 90, 1, NOW(3), NOW(3)),
       (10, NULL, 'audit', '审计日志', '/audit', 'Document', 100, 1, NOW(3), NOW(3)),
       (11, NULL, 'screen', '中控大屏', '/screen', 'DataLine', 110, 1, NOW(3), NOW(3))
ON DUPLICATE KEY UPDATE updated_at=NOW(3), enabled=VALUES(enabled);

INSERT INTO role_menu (role_id, menu_id, created_at, updated_at)
SELECT r.id, m.id, NOW(3), NOW(3)
FROM role r
         INNER JOIN menu m ON 1 = 1
WHERE r.role_key = 'ADMIN'
ON DUPLICATE KEY UPDATE updated_at = NOW(3);
