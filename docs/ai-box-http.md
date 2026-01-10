# AI 盒子（边缘告警）HTTP 对接说明

本项目已按 `add.md` 与《边缘告警.docx》新增兼容接口，用于接收 AI 盒子（边缘计算单元）推送的告警与媒体文件，并完成入库与图片落盘。

## 1. 接口前缀（重要）

文档里“服务器地址”是一个前缀，设备会在前缀后追加：

- `/device/login`
- `/device/heartBeat`
- `/device/alarm/action`
- `/device/image`
- `/device/video`

本项目兼容 3 种前缀（任选其一在盒子后台配置）：

- 空前缀：`http://47.120.48.64:9000`
- `/box`：`http://47.120.48.64:9000/box`
- `/api/school/box`：`http://47.120.48.64:9000/api/school/box`

## 2. 返回格式

兼容厂商示例：

```json
{ "code": "200", "message": "success" }
```

登录接口会额外返回 `token`（设备如不使用可忽略）。

## 3. 入库与文件落盘

### 3.1 数据入库

- `hardware_ingest_log`：记录 `/device/*` 的原始 payload（便于现场排查）
- `ai_box_device`：记录盒子序列号、最近登录/心跳
- `ai_box_alarm_action_ingest`：行为告警原始 JSON 留痕
- `ai_box_media`：图片/录像上传记录（含 `file_object_id` 与 `public_url`）
- `file_object`：落库文件对象（`storage_type=AI_BOX_PUBLIC`）

### 3.2 图片/录像存储到“文件服务器目录”

默认写入目录（可被 nginx/ftp 文件服务对外暴露）：

- `app.ai-box.file-root`（默认 `/opt/nginxTemp/html`，无权限时降级到 `./.data/ai-box-public`）

对外访问 URL 前缀（用于写入 `ai_box_media.public_url`）：

- `app.ai-box.base-url`（默认 `http://47.120.48.64:9004/`）

可通过环境变量覆盖（不必修改配置文件）：

```bash
export APP_AI_BOX_FILE_ROOT=/opt/nginxTemp/html
export APP_AI_BOX_BASE_URL=http://47.120.48.64:9004/
```

## 4. 调试示例

### 4.1 登录

```bash
curl -X POST http://localhost:9000/device/login \
  -H 'Content-Type: application/json' \
  -d '{"deviceSerial":"HQDZKETBBJHAJ0430"}'
```

### 4.2 推送行为告警（原始 JSON 留痕）

```bash
curl -X POST http://localhost:9000/device/alarm/action \
  -H 'Content-Type: application/json' \
  -d '{"deviceSerial":"HQDZKETBBJHAJ0430","alarmType":"通道堵塞","alarmTime":"2026-01-10 12:00:00"}'
```

### 4.3 图片 Base64 上传

```bash
curl -X POST http://localhost:9000/device/image \
  -H 'Content-Type: application/json' \
  -d '{"deviceSerial":"HQDZKETBBJHAJ0430","fileName":"alarm.jpg","base64":"<BASE64>"}'
```

