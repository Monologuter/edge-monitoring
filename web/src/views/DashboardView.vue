<template>
  <div>
    <div style="display: flex; align-items: baseline; justify-content: space-between; margin-bottom: 14px">
      <h2 class="sf-title">总览</h2>
      <div class="sf-chip">近 24 小时告警趋势</div>
    </div>

    <div class="sf-kpi">
      <div class="sf-card sf-kpi-item">
        <div class="sf-kpi-title">
          <span>进行中告警</span>
          <span class="sf-chip" style="border-color: rgba(255, 77, 79, 0.25); color: rgba(255, 255, 255, 0.85)">ACTIVE</span>
        </div>
        <div class="sf-kpi-value" style="color: rgba(255, 77, 79, 0.95)">{{ data?.activeAlarms ?? 0 }}</div>
      </div>
      <div class="sf-card sf-kpi-item">
        <div class="sf-kpi-title"><span>今日告警</span><span class="sf-muted">Today</span></div>
        <div class="sf-kpi-value">{{ data?.todayAlarms ?? 0 }}</div>
      </div>
      <div class="sf-card sf-kpi-item">
        <div class="sf-kpi-title"><span>在线设备</span><span class="sf-muted">Online</span></div>
        <div class="sf-kpi-value" style="color: rgba(51, 209, 122, 0.95)">{{ data?.onlineDevices ?? 0 }}</div>
      </div>
      <div class="sf-card sf-kpi-item">
        <div class="sf-kpi-title"><span>设备总量</span><span class="sf-muted">Total</span></div>
        <div class="sf-kpi-value">{{ data?.totalDevices ?? 0 }}</div>
      </div>
    </div>

    <div class="sf-card" style="margin-top: 14px; padding: 14px">
      <div ref="chartEl" style="height: 320px; width: 100%"></div>
    </div>
  </div>
</template>

<script setup lang="ts">
import { onMounted, ref } from "vue";
import * as echarts from "echarts";
import { http } from "@/api/http";

type DashboardVO = {
  activeAlarms: number;
  todayAlarms: number;
  onlineDevices: number;
  totalDevices: number;
  alarmTrend24h: number[];
};

const data = ref<DashboardVO | null>(null);
const chartEl = ref<HTMLDivElement | null>(null);

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

function render() {
  if (!chartEl.value || !data.value) return;
  const chart = echarts.init(chartEl.value);
  chart.setOption({
    backgroundColor: "transparent",
    grid: { left: 40, right: 18, top: 22, bottom: 34 },
    tooltip: { trigger: "axis" },
    xAxis: {
      type: "category",
      data: buildHours(),
      axisLabel: { color: "rgba(255,255,255,0.7)" },
      axisLine: { lineStyle: { color: "rgba(255,255,255,0.14)" } }
    },
    yAxis: {
      type: "value",
      axisLabel: { color: "rgba(255,255,255,0.7)" },
      splitLine: { lineStyle: { color: "rgba(255,255,255,0.08)" } }
    },
    series: [
      {
        name: "告警",
        type: "line",
        smooth: true,
        data: data.value.alarmTrend24h || [],
        lineStyle: { width: 3, color: "rgba(61,214,198,0.95)" },
        areaStyle: {
          color: new echarts.graphic.LinearGradient(0, 0, 0, 1, [
            { offset: 0, color: "rgba(61,214,198,0.22)" },
            { offset: 1, color: "rgba(61,214,198,0.02)" }
          ])
        },
        symbol: "circle",
        symbolSize: 8,
        itemStyle: { color: "rgba(42,166,255,0.95)" }
      }
    ]
  });
  window.addEventListener("resize", () => chart.resize());
}

onMounted(async () => {
  await load();
  render();
});
</script>

