<template>
  <div class="screen">
    <div class="top sf-card">
      <div>
        <div class="title">中控大屏</div>
        <div class="sub">实时告警 · 设备态势 · 风险趋势</div>
      </div>
      <div class="right">
        <div class="sf-chip"><span class="dot"></span><span>WebSocket 实时推送</span></div>
        <div class="sf-chip">{{ nowText }}</div>
      </div>
    </div>

    <div class="grid">
      <div class="sf-card pane">
        <div class="paneTitle">关键指标</div>
        <div class="kpis">
          <div class="kpi">
            <div class="k">进行中告警</div>
            <div class="v danger">{{ overview?.activeAlarms ?? 0 }}</div>
          </div>
          <div class="kpi">
            <div class="k">今日告警</div>
            <div class="v">{{ overview?.todayAlarms ?? 0 }}</div>
          </div>
          <div class="kpi">
            <div class="k">在线设备</div>
            <div class="v success">{{ overview?.onlineDevices ?? 0 }}</div>
          </div>
          <div class="kpi">
            <div class="k">设备总量</div>
            <div class="v">{{ overview?.totalDevices ?? 0 }}</div>
          </div>
        </div>
        <div ref="chartEl" class="chart"></div>
      </div>

      <div class="sf-card pane">
        <div class="paneTitle">实时告警（最新 20 条）</div>
        <div class="alarmList">
          <div v-for="a in alarms" :key="a.id" class="alarmItem">
            <div class="line">
              <div class="tag" :class="{ active: a.alarmStatus === 'ACTIVE' }">{{ a.alarmStatus }}</div>
              <div class="name">{{ a.alarmType }}</div>
            </div>
            <div class="meta">
              <span class="sf-muted">{{ a.deviceCode || "—" }}</span>
              <span class="sf-muted">·</span>
              <span class="sf-muted">{{ fmt(a.warningTime) }}</span>
            </div>
          </div>
        </div>
      </div>
    </div>
  </div>
</template>

<script setup lang="ts">
import { onMounted, onUnmounted, ref } from "vue";
import { ElMessage } from "element-plus";
import * as echarts from "echarts";
import { http } from "@/api/http";
import { connectStomp } from "@/realtime/stomp";

type DashboardVO = { activeAlarms: number; todayAlarms: number; onlineDevices: number; totalDevices: number; alarmTrend24h: number[] };
type AlarmVO = {
  id: number;
  alarmType: string;
  alarmStatus: string;
  workflowStatus: string;
  riskLevel: string;
  deviceCode: string | null;
  warningTime: number;
  clearTime: number | null;
  archivedTime: number | null;
  handler: string | null;
  remark: string | null;
};
type PageResponse<T> = { list: T[]; page: number; pageSize: number; total: number };

const overview = ref<DashboardVO | null>(null);
const alarms = ref<AlarmVO[]>([]);
const chartEl = ref<HTMLDivElement | null>(null);
const nowText = ref("");

let timer: number | null = null;
let chart: echarts.ECharts | null = null;
let stomp: ReturnType<typeof connectStomp> | null = null;
let sub: { unsubscribe: () => void } | null = null;

function fmt(ts: number) {
  const d = new Date(ts);
  const pad = (n: number) => String(n).padStart(2, "0");
  return `${pad(d.getHours())}:${pad(d.getMinutes())}:${pad(d.getSeconds())}`;
}

function buildHours() {
  const now = new Date();
  const hours: string[] = [];
  for (let i = 23; i >= 0; i--) {
    const d = new Date(now.getTime() - i * 3600 * 1000);
    hours.push(String(d.getHours()).padStart(2, "0") + ":00");
  }
  return hours;
}

async function load() {
  overview.value = await http.get<DashboardVO>("/api/v1/dashboard/overview");
  const data = await http.get<PageResponse<AlarmVO>>("/api/v1/alarms?page=1&pageSize=20&status=ACTIVE");
  alarms.value = data.list;
}

function render() {
  if (!chartEl.value || !overview.value) return;
  chart = echarts.init(chartEl.value);
  chart.setOption({
    backgroundColor: "transparent",
    grid: { left: 40, right: 18, top: 22, bottom: 30 },
    tooltip: { trigger: "axis" },
    xAxis: { type: "category", data: buildHours(), axisLabel: { color: "rgba(255,255,255,0.7)" }, axisLine: { lineStyle: { color: "rgba(255,255,255,0.14)" } } },
    yAxis: { type: "value", axisLabel: { color: "rgba(255,255,255,0.7)" }, splitLine: { lineStyle: { color: "rgba(255,255,255,0.08)" } } },
    series: [
      {
        name: "告警",
        type: "bar",
        data: overview.value.alarmTrend24h || [],
        itemStyle: { color: "rgba(42,166,255,0.75)" }
      }
    ]
  });
  window.addEventListener("resize", () => chart?.resize());
}

function tickNow() {
  const d = new Date();
  const pad = (n: number) => String(n).padStart(2, "0");
  nowText.value = `${d.getFullYear()}-${pad(d.getMonth() + 1)}-${pad(d.getDate())} ${pad(d.getHours())}:${pad(d.getMinutes())}:${pad(d.getSeconds())}`;
}

onMounted(async () => {
  tickNow();
  timer = window.setInterval(tickNow, 1000);
  try {
    await load();
    render();
  } catch (e: any) {
    ElMessage.error(e?.message || "加载失败");
  }

  stomp = connectStomp((e) => ElMessage.error(String(e || "WebSocket 连接失败")));
  stomp.activate();
  sub = stomp.subscribeJson<AlarmVO>("/topic/alarms", (a) => {
    // 最新告警置顶（去重）
    alarms.value = [a, ...alarms.value.filter((x) => x.id !== a.id)].slice(0, 20);
    if (a.alarmStatus === "ACTIVE") {
      void load().then(() => render()).catch(() => {});
    }
  });
});

onUnmounted(() => {
  if (timer) window.clearInterval(timer);
  sub?.unsubscribe();
  void stomp?.deactivate();
  chart?.dispose();
});
</script>

<style scoped>
.screen {
  padding: 0;
}
.top {
  padding: 14px 16px;
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 14px;
}
.title {
  font-size: 18px;
  font-weight: 750;
}
.sub {
  margin-top: 2px;
  color: var(--sf-text-2);
  font-size: 12px;
}
.right {
  display: flex;
  gap: 10px;
  align-items: center;
}
.dot {
  width: 8px;
  height: 8px;
  border-radius: 999px;
  background: var(--sf-success);
  box-shadow: 0 0 0 6px rgba(51, 209, 122, 0.1);
}
.grid {
  display: grid;
  grid-template-columns: 1.1fr 0.9fr;
  gap: 14px;
}
.pane {
  padding: 14px;
  min-height: 520px;
}
.paneTitle {
  font-size: 13px;
  color: var(--sf-text-1);
  margin-bottom: 10px;
}
.kpis {
  display: grid;
  grid-template-columns: repeat(4, minmax(0, 1fr));
  gap: 10px;
  margin-bottom: 10px;
}
.kpi {
  background: rgba(0, 0, 0, 0.12);
  border: 1px solid rgba(255, 255, 255, 0.12);
  border-radius: 12px;
  padding: 10px 10px;
}
.k {
  font-size: 12px;
  color: var(--sf-text-2);
}
.v {
  margin-top: 8px;
  font-size: 22px;
  font-weight: 750;
}
.danger {
  color: rgba(255, 77, 79, 0.95);
}
.success {
  color: rgba(51, 209, 122, 0.95);
}
.chart {
  height: 340px;
  width: 100%;
}
.alarmList {
  display: grid;
  gap: 10px;
  max-height: 470px;
  overflow: auto;
  padding-right: 6px;
}
.alarmItem {
  border: 1px solid rgba(255, 255, 255, 0.12);
  border-radius: 12px;
  padding: 10px 10px;
  background: rgba(0, 0, 0, 0.08);
}
.line {
  display: flex;
  align-items: center;
  gap: 10px;
}
.tag {
  font-size: 12px;
  padding: 2px 8px;
  border-radius: 999px;
  border: 1px solid rgba(255, 255, 255, 0.12);
  color: var(--sf-text-1);
}
.tag.active {
  border-color: rgba(255, 77, 79, 0.28);
  color: rgba(255, 255, 255, 0.9);
}
.name {
  font-size: 13px;
  font-weight: 650;
}
.meta {
  margin-top: 6px;
  display: flex;
  gap: 8px;
  font-size: 12px;
}
</style>

