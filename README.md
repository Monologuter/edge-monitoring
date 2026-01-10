# 安全监测预警系统

## 1. 项目概述 (Project Overview)

### 1.1 项目目标

安全生产事故给人民的生命、财产造成了巨大的损失，事故影响通过媒体快速扩散，安全监管一把手责任，如何降低事故发生频率、减少事故损失等问题摆在我们面前。2020年国家印发《全国安全生产专项整治三年行动计划》，要求建立公共安全隐患排查和安全预防控制体系，安全风险管控由政府推动为主向企业自主开展转变、大力推动科技创新，全面提升本质安全水平。同年，工信部、应急部、安委办联合印发《“工业互联网+安全生产”行动计划（2021-2023年）》，通过工业互联网在安全生产中的融合应用，增强工业安全生产的感知、监测、预警、处置和评估能力，加速安全生产从静态分析向动态感知、事后应急向事前预防、单点防控向全局联防的转变，提升工业生产本质安全水平。

### 1.2 "高级" 定义

- **技术栈前沿**: 采用 Spring Boot  + Vue 3 (Composition API) + TypeScript。
- **性能卓越**: 引入 Redis 多级缓存、Elasticsearch 全文检索、静态资源 CDN 加速。
- **交互丰富**: 实时通知（WebSocket）、大屏展示、实时点位预警 智能监测 人员身份监控等多种智能场景。

### 1.3 数据对象（摘要字段）

- 企业信息：companyName、businessLicense、businessLicenseFile、businessLicenseStart/End、businessLicenseScope、businessLicenseIssuingAuthority、address、registerAddress、companyStatus、dosage、reservoirArea 等
- 人员信息：personName、idcard、personType（法定代表人/负责人/安全管理/特种作业/其他）、isCertified、联系方式等
- 仓库/库房：storeNum/storeroomNum、名称、面积、危险等级（01/02/03）、核定药量、核定人员
- 设备信息：deviceCode、deviceName、deviceType（1 视频、2 红外、3 温度、4 湿度）、上下限、单位、库房/仓库编码
- 服务器信息：computerName、ip、originalId、companyCode
- 服务器心跳：定时上传（频率以省厅为准）
- 人员出入记录：idcard、personName、personType（员工/访客/其他）、inOutState（进/出）、inOutTime、imageFile
- 车辆出入记录：licensePlateNumber、carType（危化/其他）、inOutState、inOutTime、imageFile
- 温度/湿度：deviceCode、realValue、systime（同一设备每小时一条）
- 报警信息：alarmType（超员/堵塞通道/超高超量/非法入侵/遮挡偏移）、alarmStatus、warningDate、clearDate、alarmFile
- 预警信息获取：`/fws/rec/ew/com-page`，支持 riskLevel、pushType、时间范围、分页
- 预警反馈：`/fws/rec/ew/feedback`，id + feedback

## 2、项目需求

### 2.1 业务范围
- 企业信息展示
- 人员信息
- 车辆信息
- 仓库信息
- 库房信息
- 企业服务器信息
- 设备管理与传感器数据（温度、湿度、消防液位）
- 人员进出记录
- 车辆进出记录
- 传感器报警信息
- 数据关联关系建模
- 对接省厅（安全生产监测预警系统）
- 中控大屏 各个硬件设备的监控

### 2.2 涉及硬件设备（附件清单-->[设备统计及IP.xls](%E8%AE%BE%E5%A4%87%E7%BB%9F%E8%AE%A1%E5%8F%8AIP.xls)）

- 摄像头报警
  1.非法入侵报警：门禁，道闸根据已有的平台录入
  后台管理系统的人员车牌来判定，假如没
  有录入的人员车辆进入就属于非法入侵
  2.通道堵塞报警：过道有物体，AI盒子抓取
  摄像头视频流，报警信息就推送
  3.超高超重报警：物体上有超过2.5米，AI
  盒子抓取视频流判断出来，报警信息就推
  送。
  4.摄像头遮挡偏移：摄像头自带功能，通过
  Al盒子来。
  超员报警，人员定位系统提供
- 人员点位系统实时报警参数文档：https://s.apifox.cn/f3e9dfbf-d5e0-4a60-878c-3d6c97146348/6920047m0


## 3、需求流程规格
- FR-01 企业信息录入/展示，字段符合省厅规范
- FR-02 人员档案维护与人员出入记录接入
- FR-03 车辆档案维护与车辆出入记录接入
- FR-04 仓库/库房信息维护与关联
- FR-05 温湿度/液位实时采集与阈值告警
- FR-06 告警信息生成、处理、消警与记录
- FR-07 预警信息查询与反馈提交
- FR-08 设备管理：状态、配置、在线监控
- FR-09 视频接入信息管理与关联
- FR-10 省厅数据上报与结果追踪
- FR-11 预警联动：触发 MQTTX（MQTT）音柱/TTS 播报
- FR-12 报警闭环：生成 → 处理 → 消警 → 归档

## 4、Tips
- 其中除了摄像头其他的硬件数据都是通过本地串口服务器读取 然后通过网络将数据推送过来 所以我们
需要做的就是接受串口服务器发送过来的数据 然后将其进行入库操作  其中本地串口服务器的参数如下

- 摄像头的相关操作和对接案例参考如下 [摄像头边缘告警.docx](%E6%91%84%E5%83%8F%E5%A4%B4%E8%BE%B9%E7%BC%98%E5%91%8A%E8%AD%A6.docx)  [camera-java](camera-java)



- 其中MySQL使用本地的  参数如下
  url: jdbc:mysql://localhost:3306/safetyfire?useUnicode=true&characterEncoding=UTF-8&allowPublicKeyRetrieval=true&verifyServerCertificate=false&useSSL=false
  username: root
  password: 12345678

- 本地Redis参数如下   6379  

- 代码需要有中文注释  sql脚本需要有中文注释  其中[camera-java](camera-java)是硬件对接示例代码 不是业务代码

---

## 5. 本仓库新增：企业级安全预警监控系统（可运行骨架）

> 目标：在现有需求基础上，提供“可登录、可看总览、可看/处理告警、可接入硬件上报”的企业级基础能力与统一规范，便于你持续扩展 FR-01 ~ FR-12。

### 5.1 目录结构

- `server/`：后端（Spring Boot 3 + MyBatis + MySQL + Redis + JWT）
- `web/`：前端（Vue 3 + TypeScript + Vite + Pinia + Element Plus + ECharts）
- `docs/`：接口/数据库/部署/运维文档
- `camera-java/`：硬件对接示例代码（非业务代码）

### 5.2 一键依赖（可选）

如果你本机没有 MySQL/Redis，直接用 Docker 启动：

```bash
docker compose up -d
```

如果你之前启动失败（Flyway 迁移失败/重复字段/半迁移状态），一键重置本地 MySQL 8.0：

```bash
bash scripts/reset-local-mysql8.sh
```

### 5.3 启动后端（JDK 21）

本机已安装 `zulu-21.jdk` 的情况下：

```bash
export JAVA_HOME=/Library/Java/JavaVirtualMachines/zulu-21.jdk/Contents/Home
export PATH="$JAVA_HOME/bin:$PATH"

mvn -f server/pom.xml -DskipTests spring-boot:run
```

- Swagger：`http://localhost:9000/swagger-ui.html`
- 健康检查：`http://localhost:9000/actuator/health`

默认管理员账号（首次启动会自动初始化）：

- 用户名：`admin`
- 密码：`Admin@123456`

已内置测试账号（用于验证“用户/角色/数据权限”）：

- 值班员：`operator1 / Operator@123456`（默认数据范围：`C001`）
- 审计员：`auditor1 / Auditor@123456`（默认数据范围：`C002`）

### 5.4 启动前端

```bash
cd web
npm i
npm run dev
```

访问：`http://localhost:3001`

硬件上报日志（后台查看硬件推送数据）：

- 菜单：`硬件数据`
- 接口：`GET /api/v1/hardware/ingest-logs`

### 5.5 硬件数据接入（示例）

开发环境默认设备上报 Key：`DEV_KEY_001`（表 `device_api_key` 可改）
开发环境默认 Secret：`DEV_SECRET_001`（表 `device_api_key.api_secret` 可改）

协议口径（以 `硬件接口/` 为准）：

- 温度/湿度/液位：MQTT（MQTTX）上报到 `safetyfire/ingest/device-reading`
- 其他硬件：HTTP 上报到 `/api/v1/ingest/*`

设备实时值上报（会自动触发阈值告警演示）：

```bash
API_KEY="DEV_KEY_001"
SECRET="DEV_SECRET_001"
TS="$(python3 - << 'PY'
import time
print(int(time.time()*1000))
PY
)"
NONCE="$(python3 - << 'PY'
import secrets
print(secrets.token_hex(8))
PY
)"
BODY='{"deviceCode":"TEMP_001","realValue":40,"systime":1730000000000}'
BODY_SHA="$(printf "%s" "$BODY" | shasum -a 256 | awk '{print $1}')"
CANONICAL="$(printf "%s\n%s\n%s\n%s" "$API_KEY" "$TS" "$NONCE" "$BODY_SHA")"
SIG="$(printf "%s" "$CANONICAL" | openssl dgst -sha256 -hmac "$SECRET" -hex | awk '{print $2}')"

curl -X POST "http://localhost:9000/api/v1/ingest/device-reading" \
  -H "Content-Type: application/json" \
  -H "X-Device-Api-Key: $API_KEY" \
  -H "X-Timestamp: $TS" \
  -H "X-Nonce: $NONCE" \
  -H "X-Signature: $SIG" \
  -d "$BODY"
```

更多说明见：

- `docs/api.md`
- `docs/db.md`
- `docs/error-codes.md`
- `docs/runbook.md`
