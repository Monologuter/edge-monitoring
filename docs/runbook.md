# 运维手册（Runbook）

## 常见问题

### 1) 登录失败

- 确认后端已启动：`http://localhost:9000/actuator/health`
- 默认管理员：`admin / Admin@123456`（首次登录后尽快修改）
- 测试账号：`operator1 / Operator@123456`、`auditor1 / Auditor@123456`

### 2) 前端提示未登录/反复跳转

- 确认后端 `app.jwt.secret` 已设置且长度 ≥ 32
- 清理浏览器本地缓存（LocalStorage）后重试

### 3) 硬件上报返回鉴权失败

- Header 必须带：`X-Device-Api-Key`
- 开发环境默认 Key：`DEV_KEY_001`（表 `device_api_key` 可配置）
- 签名 Header：`X-Timestamp` / `X-Nonce` / `X-Signature`（见 `docs/api.md`）
- 默认 secret：`DEV_SECRET_001`（表 `device_api_key.api_secret`）

### 0) 后端启动报 Flyway 迁移失败（重复字段/表已存在）

原因：你连接了“已有数据的数据库”，但本项目默认按“干净库”执行迁移。

推荐处理：用一个全新的 `safetyfire` 数据库（本项目自带 `docker compose` 的 MySQL 8.0）。

#### A. Docker（推荐，一键清空并重建本地 MySQL 8.0 数据）

> 注意：会清空你本机项目目录下的 `./.data/mysql`（仅影响本项目的 Docker MySQL 数据）。

```bash
docker compose down
rm -rf ./.data/mysql
docker compose up -d
```

#### B. SQL（手工重置数据库）

```sql
DROP DATABASE IF EXISTS safetyfire;
CREATE DATABASE safetyfire DEFAULT CHARACTER SET utf8mb4;
```

#### C. 如果报错是 “Validate failed: Detected failed migration …”

说明：之前已经执行过迁移但中途失败，`flyway_schema_history` 留下了失败记录。

推荐仍按 **A/B** 方式重置为干净库；如果你确定要在当前库继续修复，可在数据库中执行：

```sql
DELETE FROM flyway_schema_history WHERE success = 0;
```

### 6) MQTT 硬件接入不工作（MQTTX）

- 后端要打开：`app.mqtt.enabled=true` 且 `app.mqtt.ingest.enabled=true`
- broker 地址：`app.mqtt.broker-url`（示例 `tcp://127.0.0.1:1883`）
- MQTTX 发布到：`app.mqtt.ingest.topic-prefix` 下的 `device-reading` topic（见 `docs/api.md`）
- payload 必须是 `MqttIngestEnvelope`，其中 `data` 为业务 JSON 字符串，并按规则签名

### 4) WebSocket 不工作

- 后端 WebSocket：`ws://localhost:9000/ws`
- 前端会在连接时携带 `Authorization: Bearer <accessToken>`（需要已登录）
- 如果被浏览器拦截，检查前端 `web/index.html` 的 CSP `connect-src` 是否包含 `ws://localhost:9000`

### 5) MQTT 音柱/TTS 不生效（MQTTX）

- 先确认 broker 可用：`app.mqtt.enabled=true` 且 `app.mqtt.broker-url` 正确
- `linkageType` 用 `MQTT_TTS`，`target` 填 topic（可选 `|qos`）
- 通过 MQTTX 订阅同一 topic 验证是否收到 payload

## 监控与指标

- 健康检查：`/actuator/health`
- Prometheus：`/actuator/prometheus`
