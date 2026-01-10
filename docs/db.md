# 数据库说明（MySQL）

## 连接信息（本地默认）

- `jdbc:mysql://localhost:3306/safetyfire`
- 用户名：`root`
- 密码：`12345678`

## 初始化脚本

- `server/src/main/resources/db/migration/V1__init.sql`
- `server/src/main/resources/db/migration/V2__business.sql`
- `server/src/main/resources/db/migration/V3__device_security.sql`

## 核心表（当前版本）

- `user_account`：用户账户（BCrypt）
- `role` / `user_role`：角色体系（用于 RBAC 扩展）
- `device`：设备（阈值、位置、在线状态）
- `device_api_key`：硬件上报 API Key（开发环境预置 `DEV_KEY_001`）
- `alarm_event`：告警事件（告警闭环：生成 → 处理 → 消警）
