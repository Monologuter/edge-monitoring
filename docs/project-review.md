# 项目审查与现状分析（edge-monitoring）

> 范围：本仓库 `server/`（Spring Boot 3 + MyBatis + MySQL + Redis + JWT）与 `web/`（Vue3 + TS + Vite + Pinia）。

## 1. 仓库结构与交付物

- 后端：`server/`（Maven 单模块，打包 `monitor-server-0.1.0.jar`）
- 前端：`web/`（Vite 构建产物 `web/dist/`）
- 文档：`docs/`（API/DB/部署/运维）
- 基础部署：`docker-compose.yml`（MySQL/Redis 等依赖在本机/服务器侧启动）

## 2. 后端架构画像（server/）

### 2.1 核心能力

- 鉴权：JWT 无状态（`server/src/main/java/com/safetyfire/monitor/security/*`）
- 数据库迁移：Flyway（`server/src/main/resources/db/migration/*`）
- 数据访问：MyBatis XML（`server/src/main/resources/mapper/*.xml`）
- 硬件数据接入：
  - 统一接入：`/api/v1/ingest/*`（使用 `X-Device-Api-Key` 设备鉴权）
  - 原始数据留痕：`hardware_ingest_log`（`V6__hardware_ingest_log.sql`）

### 2.2 关键配置（默认）

配置文件：`server/src/main/resources/application.yml`

- 端口：`server.port=9000`
- MySQL：默认连接 `47.120.48.64:3306/safetyfire`
- Redis：默认 `47.120.48.64:6379`
- MQTT：默认 `47.120.48.64:1883`

### 2.3 主要数据表（节选）

- 用户/角色/菜单：`user_account`、`role`、`menu`、`role_menu`
- 企业/仓库/库房/设备：`company`、`store`、`storeroom`、`device`
- 告警闭环：`alarm_event`
- 文件对象：`file_object`（`/api/v1/files/*`）
- 硬件上报留痕：`hardware_ingest_log`
- AI 盒子告警：`ai_box_alarm`、`person_overcrowd`（`V8__ai_box_and_overcrowd.sql`）

## 3. 前端架构画像（web/）

- 技术栈：Vue 3 + TypeScript + Vite + Pinia + Vue Router + Element Plus + ECharts
- 构建脚本：`web/package.json`
  - `npm run dev`（开发）
  - `npm run build`（生产构建）

## 4. 风险点与改进建议（按优先级）

- `application.yml` 中 `app.jwt.secret` 为示例值，生产必须通过环境变量注入强随机密钥（避免泄露导致全站被伪造 token）。
- 数据库/Redis/MQTT 默认指向公网 IP，建议：
  - 生产使用内网地址或安全组白名单
  - 打开最小权限账号与连接加密策略
- `Flyway validate-on-migrate: false` + `baseline-on-migrate: true` 组合会弱化迁移一致性校验；生产建议开启校验并严格管理变更流程（避免半迁移状态造成不可预期行为）。

## 5. add.md 需求落地（边缘告警对接）

已新增 AI 盒子 HTTP 推送接口，兼容厂商文档中的路径与返回格式，并完成：

- 告警/心跳/登录等请求的“原始数据入库留痕”
- 图片/录像：Base64 → 二进制 → 落盘到“文件服务目录” → 写入 `file_object` 与关联表
- 对外可访问 URL 写入 `ai_box_media.public_url`（默认使用 `http://47.120.48.64:9004/`）

对接说明见：`docs/ai-box-http.md`

