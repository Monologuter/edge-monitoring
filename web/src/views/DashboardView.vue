<template>
  <div class="sf-page dashboard">
    <div class="sf-page-head">
      <div>
        <h2 class="sf-page-title">数据看板</h2>
        <div class="sf-page-sub">{{ data?.companyName || "—" }} · 许可证编号：{{ data?.businessLicense || "—" }}</div>
      </div>
      <div class="sf-page-actions">
        <span class="sf-badge">近 24 小时告警趋势</span>
      </div>
    </div>

    <div class="sf-kpi sf-stagger">
      <div class="sf-card sf-kpi-item kpiAlarm" style="--i: 40ms">
        <div class="sf-kpi-head">
          <div>
            <div class="sf-kpi-title">今日预警总数</div>
            <div class="sf-kpi-sub">ALARM TODAY</div>
          </div>
          <div class="sf-kpi-icon kpiAlarm">
            <el-icon><Warning /></el-icon>
          </div>
        </div>
        <div class="sf-kpi-value">{{ data?.todayAlarms ?? 0 }}</div>
      </div>
      <div class="sf-card sf-kpi-item kpiOnline" style="--i: 80ms">
        <div class="sf-kpi-head">
          <div>
            <div class="sf-kpi-title">在线设备</div>
            <div class="sf-kpi-sub">ACTIVE</div>
          </div>
          <div class="sf-kpi-icon kpiOnline">
            <el-icon><Cpu /></el-icon>
          </div>
        </div>
        <div class="sf-kpi-value">{{ data?.onlineDevices ?? 0 }}</div>
      </div>
      <div class="sf-card sf-kpi-item kpiTotal" style="--i: 120ms">
        <div class="sf-kpi-head">
          <div>
            <div class="sf-kpi-title">设备总数</div>
            <div class="sf-kpi-sub">TOTAL</div>
          </div>
          <div class="sf-kpi-icon kpiTotal">
            <el-icon><Monitor /></el-icon>
          </div>
        </div>
        <div class="sf-kpi-value">{{ data?.totalDevices ?? 0 }}</div>
      </div>
      <div class="sf-card sf-kpi-item kpiRate" style="--i: 160ms">
        <div class="sf-kpi-head">
          <div>
            <div class="sf-kpi-title">在线率</div>
            <div class="sf-kpi-sub">AVAILABILITY</div>
          </div>
          <div class="sf-kpi-icon kpiRate">
            <el-icon><TrendCharts /></el-icon>
          </div>
        </div>
        <div class="sf-kpi-value">{{ (data?.onlineRate ?? 0).toFixed(1) }}%</div>
      </div>
    </div>

    <div class="grid">
      <div class="sf-card panel">
        <div class="panelTitle">环境与现场概况</div>
        <div class="stats">
          <div class="statItem">
            <div class="statLabel">温度</div>
            <div class="statValue">
              {{ data?.temperatureValue == null ? "-" : `${data.temperatureValue}℃` }}
            </div>
          </div>
          <div class="statItem">
            <div class="statLabel">湿度</div>
            <div class="statValue">
              {{ data?.humidityValue == null ? "-" : `${data.humidityValue}%` }}
            </div>
          </div>
          <div class="statItem">
            <div class="statLabel">液位</div>
            <div class="statValue">
              {{ data?.levelValue == null ? "-" : `${data.levelValue}m` }}
            </div>
          </div>
          <div class="statItem">
            <div class="statLabel">在厂人数</div>
            <div class="statValue">{{ data?.onsitePeople ?? 0 }}</div>
          </div>
          <div class="statItem">
            <div class="statLabel">在厂车辆</div>
            <div class="statValue">{{ data?.onsiteCars ?? 0 }}</div>
          </div>
          <div class="statItem">
            <div class="statLabel">库房数量</div>
            <div class="statValue">{{ data?.storeroomCount ?? 0 }}</div>
          </div>
        </div>
        <div class="flow">
          <div class="flowItem">
            <div class="flowLabel">今日进入人数</div>
            <div class="flowValue">{{ data?.todayInPeople ?? 0 }}</div>
          </div>
          <div class="flowItem">
            <div class="flowLabel">今日出去人数</div>
            <div class="flowValue">{{ data?.todayOutPeople ?? 0 }}</div>
          </div>
          <div class="flowItem">
            <div class="flowLabel">今日进入车辆</div>
            <div class="flowValue">{{ data?.todayInCars ?? 0 }}</div>
          </div>
          <div class="flowItem">
            <div class="flowLabel">今日出去车辆</div>
            <div class="flowValue">{{ data?.todayOutCars ?? 0 }}</div>
          </div>
        </div>
      </div>

      <div class="sf-card panel">
        <div class="panelTitle">设备在线情况</div>
        <div ref="deviceChartEl" class="chartSmall"></div>
      </div>
    </div>

    <div class="sf-card panel chartLarge">
      <div class="panelTitle">报警趋势示意图</div>
      <div ref="chartEl" class="chartLargeInner"></div>
    </div>
  </div>
</template>

<script setup lang="ts">
import { onMounted, ref } from "vue";
import * as echarts from "echarts";
import { http } from "@/api/http";
import { Cpu, Monitor, TrendCharts, Warning } from "@element-plus/icons-vue";

type DashboardVO = {
  companyName: string | null;
  businessLicense: string | null;
  temperatureValue: number | null;
  humidityValue: number | null;
  levelValue: number | null;
  onsitePeople: number;
  onsiteCars: number;
  storeroomCount: number;
  todayInPeople: number;
  todayOutPeople: number;
  todayInCars: number;
  todayOutCars: number;
  activeAlarms: number;
  todayAlarms: number;
  onlineDevices: number;
  totalDevices: number;
  onlineRate: number;
  alarmTrend24h: number[];
};

const data = ref<DashboardVO | null>(null);
const chartEl = ref<HTMLDivElement | null>(null);
const deviceChartEl = ref<HTMLDivElement | null>(null);

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
  data.value = await http.get<DashboardVO>("/api/v1/dashboard/overview");
}

function renderAlarmTrend() {
  if (!chartEl.value || !data.value) return;
  const chart = echarts.init(chartEl.value);
  chart.setOption({
    backgroundColor: "transparent",
    grid: { left: 40, right: 18, top: 22, bottom: 34 },
    tooltip: { trigger: "axis" },
    xAxis: {
      type: "category",
      data: buildHours(),
      axisLabel: { color: "rgba(60, 73, 99, 0.6)" },
      axisLine: { lineStyle: { color: "rgba(70, 88, 124, 0.12)" } }
    },
    yAxis: {
      type: "value",
      axisLabel: { color: "rgba(60, 73, 99, 0.6)" },
      splitLine: { lineStyle: { color: "rgba(70, 88, 124, 0.08)" } }
    },
    series: [
      {
        name: "告警",
        type: "line",
        smooth: true,
        data: data.value.alarmTrend24h || [],
        lineStyle: { width: 3, color: "rgba(39,183,167,0.95)" },
        areaStyle: {
          color: new echarts.graphic.LinearGradient(0, 0, 0, 1, [
            { offset: 0, color: "rgba(39,183,167,0.24)" },
            { offset: 1, color: "rgba(39,183,167,0.02)" }
          ])
        },
        symbol: "circle",
        symbolSize: 8,
        itemStyle: { color: "rgba(47,107,255,0.95)" }
      }
    ]
  });
  window.addEventListener("resize", () => chart.resize());
}

function renderDeviceStatus() {
  if (!deviceChartEl.value || !data.value) return;
  const chart = echarts.init(deviceChartEl.value);
  const offline = Math.max(0, (data.value.totalDevices || 0) - (data.value.onlineDevices || 0));
  chart.setOption({
    backgroundColor: "transparent",
    tooltip: { trigger: "item" },
    series: [
      {
        type: "pie",
        radius: ["55%", "75%"],
        avoidLabelOverlap: false,
        label: { show: false },
        labelLine: { show: false },
        data: [
          { value: data.value.onlineDevices || 0, name: "在线", itemStyle: { color: "rgba(47,107,255,0.9)" } },
          { value: offline, name: "离线", itemStyle: { color: "rgba(255,107,107,0.7)" } }
        ]
      }
    ]
  });
  window.addEventListener("resize", () => chart.resize());
}

onMounted(async () => {
  await load();
  renderAlarmTrend();
  renderDeviceStatus();
});
</script>

<style scoped>
.dashboard {
  display: grid;
  gap: 16px;
}

.grid {
  display: grid;
  grid-template-columns: 2fr 1fr;
  gap: 16px;
}

.panel {
  padding: 16px;
}

.panelTitle {
  font-size: 14px;
  font-weight: 600;
  color: var(--sf-text-0);
  margin-bottom: 12px;
}

.sf-kpi-head {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 10px;
}

.sf-kpi-sub {
  margin-top: 4px;
  font-size: 11px;
  letter-spacing: 0.6px;
  color: var(--sf-text-2);
}

.sf-kpi-icon {
  width: 36px;
  height: 36px;
  border-radius: 12px;
  display: grid;
  place-items: center;
  font-size: 18px;
  color: #ffffff;
  background: linear-gradient(135deg, rgba(47, 107, 255, 0.9), rgba(39, 183, 167, 0.85));
  box-shadow: 0 10px 18px rgba(47, 107, 255, 0.18);
}

.kpiAlarm .sf-kpi-value {
  color: rgba(255, 107, 107, 0.95);
}

.kpiAlarm .sf-kpi-icon {
  background: linear-gradient(135deg, rgba(255, 107, 107, 0.95), rgba(243, 178, 90, 0.9));
}

.kpiOnline .sf-kpi-value {
  color: rgba(39, 183, 167, 0.95);
}

.kpiOnline .sf-kpi-icon {
  background: linear-gradient(135deg, rgba(39, 183, 167, 0.95), rgba(47, 107, 255, 0.8));
}

.kpiTotal .sf-kpi-value {
  color: rgba(47, 107, 255, 0.95);
}

.kpiRate .sf-kpi-value {
  color: rgba(75, 91, 117, 0.95);
}

.stats {
  display: grid;
  grid-template-columns: repeat(3, minmax(0, 1fr));
  gap: 12px;
}

.statItem {
  padding: 12px;
  border-radius: 12px;
  background: linear-gradient(135deg, rgba(248, 250, 252, 0.9), rgba(234, 240, 247, 0.75));
  border: 1px solid rgba(15, 42, 74, 0.08);
  box-shadow: inset 0 0 0 1px rgba(255, 255, 255, 0.65);
}

.statLabel {
  color: var(--sf-text-2);
  font-size: 12px;
}

.statValue {
  margin-top: 6px;
  font-size: 18px;
  font-weight: 600;
}

.flow {
  display: grid;
  grid-template-columns: repeat(4, minmax(0, 1fr));
  gap: 12px;
  margin-top: 16px;
}

.flowItem {
  padding: 10px 12px;
  border-radius: 10px;
  background: rgba(47, 107, 255, 0.08);
  border: 1px solid rgba(47, 107, 255, 0.16);
}

.flowLabel {
  color: var(--sf-text-2);
  font-size: 12px;
}

.flowValue {
  margin-top: 6px;
  font-size: 16px;
  font-weight: 600;
}

.chartLarge {
  padding: 16px;
}

.chartLargeInner {
  height: 320px;
  width: 100%;
}

.chartSmall {
  height: 240px;
  width: 100%;
}

@media (max-width: 1100px) {
  .grid {
    grid-template-columns: 1fr;
  }
  .stats {
    grid-template-columns: repeat(2, minmax(0, 1fr));
  }
  .flow {
    grid-template-columns: repeat(2, minmax(0, 1fr));
  }
}

@media (max-width: 720px) {
  .stats {
    grid-template-columns: 1fr;
  }
  .flow {
    grid-template-columns: 1fr;
  }
}
</style>
