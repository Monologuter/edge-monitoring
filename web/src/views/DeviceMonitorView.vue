<template>
  <div>
    <div style="display: flex; align-items: baseline; justify-content: space-between; margin-bottom: 14px">
      <h2 class="sf-title">实时设备监控</h2>
      <div style="display: flex; gap: 10px; align-items: center">
        <el-select v-model="selectedDeviceCode" placeholder="选择设备" clearable filterable style="width: 240px" @change="onDeviceChange">
          <el-option v-for="d in devices" :key="d.deviceCode" :label="`${d.deviceName} (${d.deviceCode})`" :value="d.deviceCode" />
        </el-select>
        <el-button @click="loadDevices">刷新</el-button>
      </div>
    </div>

    <!-- 设备卡片网格 -->
    <div v-if="!selectedDeviceCode" class="device-grid">
      <div v-for="d in devices" :key="d.deviceCode" class="device-card" :class="{ offline: d.onlineStatus !== 1 }">
        <div class="device-card-header">
          <span class="device-name">{{ d.deviceName }}</span>
          <span class="device-status" :class="{ online: d.onlineStatus === 1 }">
            {{ d.onlineStatus === 1 ? '在线' : '离线' }}
          </span>
        </div>
        <div class="device-card-body">
          <div class="device-code">{{ d.deviceCode }}</div>
          <div class="device-location">{{ d.locationName || '-' }}</div>
          <div class="device-type">{{ getDeviceTypeName(d.deviceType) }}</div>
        </div>
        <div class="device-card-footer">
          <div class="reading-value">
            <span class="value">{{ getCurrentReading(d.deviceCode) }}</span>
            <span class="unit">{{ d.unit || '' }}</span>
          </div>
          <div class="threshold-info">
            <span class="threshold">下限: {{ d.lowerLimit ?? '-' }}</span>
            <span class="threshold">上限: {{ d.upperLimit ?? '-' }}</span>
          </div>
        </div>
      </div>
    </div>

    <!-- 选中设备详情 -->
    <div v-else class="device-detail">
      <el-button @click="selectedDeviceCode = ''" style="margin-bottom: 16px">返回列表</el-button>
      <div class="detail-card">
        <div class="detail-header">
          <h3>{{ selectedDevice?.deviceName }}</h3>
          <span class="status-badge" :class="{ online: selectedDevice?.onlineStatus === 1 }">
            {{ selectedDevice?.onlineStatus === 1 ? '在线' : '离线' }}
          </span>
        </div>
        <div class="detail-info">
          <div class="info-item"><span class="label">设备编码:</span> <span>{{ selectedDevice?.deviceCode }}</span></div>
          <div class="info-item"><span class="label">位置:</span> <span>{{ selectedDevice?.locationName || '-' }}</span></div>
          <div class="info-item"><span class="label">类型:</span> <span>{{ selectedDevice?.deviceType !== undefined ? getDeviceTypeName(selectedDevice.deviceType) : '-' }}</span></div>
          <div class="info-item"><span class="label">阈值:</span> <span>{{ selectedDevice?.lowerLimit ?? '-' }} ~ {{ selectedDevice?.upperLimit ?? '-' }} {{ selectedDevice?.unit || '' }}</span></div>
        </div>
        <div class="detail-chart">
          <div ref="chartEl" style="height: 320px; width: 100%"></div>
        </div>
      </div>
    </div>
  </div>
</template>

<script setup lang="ts">
import { onMounted, onUnmounted, ref } from "vue";
import { ElMessage } from "element-plus";
import { http } from "@/api/http";
import { connectStomp } from "@/realtime/stomp";
import * as echarts from "echarts";

type DeviceVO = {
  id: number;
  deviceCode: string;
  deviceName: string;
  deviceType: number;
  unit: string | null;
  lowerLimit: number | null;
  upperLimit: number | null;
  locationName: string | null;
  onlineStatus: number;
};

type DeviceReading = {
  deviceCode: string;
  realValue: number;
  systime: number;
};

const devices = ref<DeviceVO[]>([]);
const selectedDeviceCode = ref<string>("");
const selectedDevice = ref<DeviceVO | null>(null);
const readings = ref<Map<string, DeviceReading[]>>(new Map());
const chartEl = ref<HTMLElement | null>(null);
let chart: echarts.ECharts | null = null;
let stomp: ReturnType<typeof connectStomp> | null = null;
let sub: { unsubscribe: () => void } | null = null;

function getDeviceTypeName(type: number): string {
  const types: Record<number, string> = {
    1: "视频",
    2: "红外",
    3: "温度",
    4: "湿度",
    5: "液位"
  };
  return types[type] || "未知";
}

function getCurrentReading(deviceCode: string): string {
  const arr = readings.value.get(deviceCode);
  if (!arr || arr.length === 0) return "-";
  return arr[arr.length - 1].realValue.toFixed(1);
}

async function loadDevices() {
  try {
    const data = await http.get<{ list: DeviceVO[]; total: number }>("/api/v1/devices?page=1&pageSize=100");
    devices.value = data.list;
  } catch (e: any) {
    ElMessage.error(e?.message || "加载设备失败");
  }
}

function onDeviceChange() {
  if (selectedDeviceCode.value) {
    selectedDevice.value = devices.value.find(d => d.deviceCode === selectedDeviceCode.value) || null;
    loadTrendData();
  }
}

async function loadTrendData() {
  if (!selectedDeviceCode.value || !chartEl.value) return;

  try {
    const now = Date.now();
    const hourStart = now - (now % 3600000);
    const data = await http.get<{ list: { hourStart: number; sampleValue: number }[] }>(
      `/api/v1/device-readings/by-device/${selectedDeviceCode.value}?hours=24`
    );

    if (!chart) {
      chart = echarts.init(chartEl.value);
    }

    const option = {
      title: { text: "24小时趋势", left: "center" },
      tooltip: { trigger: "axis" },
      xAxis: {
        type: "category",
        data: data.list.map((d: { hourStart: number }) => new Date(d.hourStart).getHours() + ":00")
      },
      yAxis: { type: "value" },
      series: [{
        data: data.list.map((d: { sampleValue: number }) => d.sampleValue),
        type: "line",
        smooth: true,
        areaStyle: { opacity: 0.3 }
      }]
    };

    chart.setOption(option);
  } catch (e: any) {
    console.error("加载趋势数据失败", e);
  }
}

function setupRealtimeSubscription() {
  stomp = connectStomp(() => {});
  stomp.activate();
  sub = stomp.subscribeJson<DeviceReading>("/topic/device-readings", (reading) => {
    const arr = readings.value.get(reading.deviceCode) || [];
    arr.push(reading);
    // 只保留最近100条
    if (arr.length > 100) arr.shift();
    readings.value.set(reading.deviceCode, arr);

    // 如果正在查看该设备的详情，更新图表
    if (selectedDeviceCode.value === reading.deviceCode) {
      loadTrendData();
    }
  });
}

onMounted(() => {
  loadDevices();
  setupRealtimeSubscription();
});

onUnmounted(() => {
  sub?.unsubscribe();
  stomp?.deactivate();
  chart?.dispose();
});
</script>

<style scoped>
.device-grid {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(280px, 1fr));
  gap: 16px;
}

.device-card {
  border: 1px solid rgba(255, 255, 255, 0.12);
  border-radius: 8px;
  padding: 16px;
  transition: all 0.3s;
}

.device-card:hover {
  border-color: rgba(64, 158, 255, 0.5);
  box-shadow: 0 4px 12px rgba(0, 0, 0, 0.15);
}

.device-card.offline {
  opacity: 0.6;
}

.device-card-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 12px;
}

.device-name {
  font-weight: 600;
  font-size: 16px;
}

.device-status {
  padding: 2px 8px;
  border-radius: 4px;
  font-size: 12px;
  background: rgba(255, 255, 255, 0.1);
}

.device-status.online {
  background: rgba(51, 209, 122, 0.2);
  color: #33d17a;
}

.device-card-body {
  margin-bottom: 12px;
}

.device-code {
  font-size: 13px;
  color: rgba(255, 255, 255, 0.7);
  margin-bottom: 4px;
}

.device-location {
  font-size: 14px;
  margin-bottom: 4px;
}

.device-type {
  font-size: 12px;
  color: rgba(255, 255, 255, 0.5);
}

.device-card-footer {
  border-top: 1px solid rgba(255, 255, 255, 0.1);
  padding-top: 12px;
}

.reading-value {
  font-size: 24px;
  font-weight: 600;
  margin-bottom: 8px;
}

.reading-value .value {
  color: #409eff;
}

.reading-value .unit {
  font-size: 14px;
  color: rgba(255, 255, 255, 0.6);
  margin-left: 4px;
}

.threshold-info {
  display: flex;
  justify-content: space-between;
  font-size: 12px;
  color: rgba(255, 255, 255, 0.5);
}

.detail-card {
  border: 1px solid rgba(255, 255, 255, 0.12);
  border-radius: 8px;
  padding: 24px;
}

.detail-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 20px;
}

.detail-header h3 {
  margin: 0;
}

.status-badge {
  padding: 4px 12px;
  border-radius: 4px;
  font-size: 12px;
  background: rgba(255, 255, 255, 0.1);
}

.status-badge.online {
  background: rgba(51, 209, 122, 0.2);
  color: #33d17a;
}

.detail-info {
  display: grid;
  grid-template-columns: repeat(2, 1fr);
  gap: 12px;
  margin-bottom: 24px;
}

.info-item {
  display: flex;
}

.info-item .label {
  color: rgba(255, 255, 255, 0.6);
  margin-right: 8px;
  min-width: 80px;
}
</style>
