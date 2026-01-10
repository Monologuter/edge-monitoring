# API 说明

## Swagger

- 地址：`http://localhost:9000/swagger-ui.html`

## 认证

- 登录：`POST /api/v1/auth/login`
- 刷新：`POST /api/v1/auth/refresh`
- 当前用户：`GET /api/v1/auth/me`
- 修改密码：`POST /api/v1/auth/change-password`

## 首页

- 总览：`GET /api/v1/dashboard/overview`

## 告警中心

- 分页查询：`GET /api/v1/alarms?page=1&pageSize=20&status=ACTIVE`
- 处理/消警：`POST /api/v1/alarms/handle`
- 实时推送（WebSocket/STOMP）：`ws://localhost:9000/ws` 订阅 `/topic/alarms`

## 设备

- 分页查询：`GET /api/v1/devices?page=1&pageSize=20`
- 新增：`POST /api/v1/devices`
- 更新：`PUT /api/v1/devices`
- 删除：`DELETE /api/v1/devices/{id}`
- 小时趋势：`GET /api/v1/readings/hourly?deviceCode=TEMP_001&hours=24`

## 企业/人员/车辆/仓库/库房/视频

- 企业：`/api/v1/companies`
- 人员：`/api/v1/people`，出入记录：`/api/v1/person-inout`
- 车辆：`/api/v1/cars`，出入记录：`/api/v1/car-inout`
- 仓库：`/api/v1/stores`，库房：`/api/v1/storerooms`
- 摄像头：`/api/v1/cameras`

## 省厅对接

- 预警列表：`GET /api/v1/provincial/warnings`
- 反馈列表：`GET /api/v1/provincial/feedback`
- 提交反馈：`POST /api/v1/provincial/feedback`

## 联动（音柱/TTS，MQTTX）

- 联动事件列表：`GET /api/v1/linkage/events`
- 创建联动事件：`POST /api/v1/linkage/events`

### MQTT 音柱/TTS（MQTTX）

- `linkageType`：`MQTT_TTS`
- `target`：MQTT topic（可选 `|qos`，如 `tts/topic|1`）
- `payload`：TTS 文本

> 兼容：`linkageType=IP_TTS` 会按 MQTT_TTS 处理（历史命名）。


## 审计日志

- 列表：`GET /api/v1/audit-logs`

## 管理后台（用户/角色/数据权限）

> 需要权限：`admin:manage`（默认 ADMIN 具备）

- 用户：`GET/POST/PUT/DELETE /api/v1/admin/users`
- 重置密码：`POST /api/v1/admin/users/reset-password`
- 角色：`GET/POST/DELETE /api/v1/admin/roles`
- 权限元数据：`GET /api/v1/admin/roles/meta/permissions`
- 菜单元数据：`GET /api/v1/admin/roles/meta/menus`

## 数据接入（硬件上报）

协议口径（以 `硬件接口/` 为准）：

- 温度/湿度/液位：MQTT（MQTTX）→ `safetyfire/ingest/device-reading`
- 摄像头/AI 盒子告警：HTTP → `POST /api/v1/ingest/alarm`
- 门禁/人脸识别记录：HTTP → `POST /api/v1/ingest/person-inout`
- 道闸/车牌识别记录：HTTP → `POST /api/v1/ingest/car-inout`
- 企业服务器心跳：HTTP → `POST /api/v1/ingest/server-heartbeat`

> Header（开发环境默认）：
> - `X-Device-Api-Key: DEV_KEY_001`
> - `X-Timestamp: <毫秒时间戳>`
> - `X-Nonce: <随机串>`
> - `X-Signature: <hex>`（HMAC-SHA256）
>
> 签名串：`apiKey + "\n" + timestamp + "\n" + nonce + "\n" + sha256(body)`
>  
> secret：`device_api_key.api_secret`（开发默认 `DEV_SECRET_001`）

- 设备实时值：`POST /api/v1/ingest/device-reading`
- 告警上报：`POST /api/v1/ingest/alarm`
- 人员出入：`POST /api/v1/ingest/person-inout`
- 车辆出入：`POST /api/v1/ingest/car-inout`
- 服务器心跳：`POST /api/v1/ingest/server-heartbeat`

## MQTT 硬件接入（推荐，MQTTX）

> 统一包络：`MqttIngestEnvelope`（见 `server/src/main/java/com/safetyfire/monitor/domain/dto/MqttIngestEnvelope.java:1`）
>
> 注意：`data` 是“业务 JSON 字符串”，不是对象，便于各类硬件稳定计算签名。
>
> 协议口径：仅 **温度/湿度/液位** 等“设备实时值”走 MQTT；其他硬件设备走 HTTP（见上面的 `/api/v1/ingest/*`）。

### 配置

- `app.mqtt.enabled=true`
- `app.mqtt.ingest.enabled=true`
- `app.mqtt.broker-url=tcp://127.0.0.1:1883`
- `app.mqtt.ingest.topic-prefix=safetyfire/ingest`

### Topic

- `safetyfire/ingest/device-reading`

### 签名（与 HTTP 一致，但 body 替换为 data 字符串）

- `bodySha256 = sha256(UTF-8(data))`
- `canonical = apiKey + "\n" + timestamp + "\n" + nonce + "\n" + bodySha256`
- `signature = hex(hmac_sha256(secret, canonical))`

### MQTTX 发布示例（device-reading）

向 topic `safetyfire/ingest/device-reading` 发布 payload（示例）：

```json
{
  "apiKey": "DEV_KEY_001",
  "timestamp": 1730000000000,
  "nonce": "a1b2c3d4",
  "signature": "请按上面规则生成",
  "data": "{\"deviceCode\":\"TEMP_001\",\"realValue\":40,\"systime\":1730000000000}"
}
```

## 硬件上报日志（后台排查）

> 用于在后台查看“硬件传递过来的原始数据”，覆盖 MQTT/HTTP 两种接入方式。

- 列表：`GET /api/v1/hardware/ingest-logs?page=1&pageSize=20`
- 支持过滤：`channel/messageType/apiKey/companyCode/ok/topicLike`
