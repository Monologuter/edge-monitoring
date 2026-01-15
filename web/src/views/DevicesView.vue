<template>
  <div class="sf-page">
    <div class="sf-page-head">
      <div>
        <h2 class="sf-page-title">设备管理</h2>
        <div class="sf-page-sub">设备档案、阈值与在线状态</div>
      </div>
      <div class="sf-page-actions">
        <el-select v-model="deviceTypeFilter" placeholder="设备类型" style="width: 160px" clearable>
          <el-option v-for="t in deviceTypeOptions" :key="t.value" :label="t.label" :value="t.value" />
        </el-select>
        <el-button type="primary" @click="openCreate">新增设备</el-button>
        <el-button @click="load">刷新</el-button>
      </div>
    </div>

    <div class="sf-card sf-table-card">
      <el-table :data="list" style="width: 100%" v-loading="loading">
      <el-table-column prop="companyCode" label="企业编码" min-width="140" />
      <el-table-column prop="deviceCode" label="编码" min-width="140" />
      <el-table-column prop="deviceName" label="名称" min-width="180" />
      <el-table-column prop="deviceType" label="类型" width="110">
        <template #default="{ row }">
          <span class="sf-chip">{{ deviceTypeLabel(row.deviceType) }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="locationName" label="位置" min-width="160" />
      <el-table-column prop="storeNum" label="仓库编码" min-width="140" />
      <el-table-column prop="storeroomNum" label="库房编码" min-width="140" />
      <el-table-column prop="ipAddress" label="IP地址" min-width="140" />
      <el-table-column prop="accessUsername" label="账号" width="120">
        <template #default="{ row }">
          <span class="sf-muted">{{ row.accessUsername ? "••••••" : "-" }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="accessPassword" label="密码" width="120">
        <template #default="{ row }">
          <span class="sf-muted">{{ row.accessPassword ? "••••••" : "-" }}</span>
        </template>
      </el-table-column>
      <el-table-column label="阈值" min-width="160">
        <template #default="{ row }">
          <span class="sf-muted">{{ row.lowerLimit ?? "-" }}</span>
          <span class="sf-muted"> ~ </span>
          <span class="sf-muted">{{ row.upperLimit ?? "-" }}</span>
          <span class="sf-muted"> {{ row.unit || "" }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="onlineStatus" label="在线" width="90">
        <template #default="{ row }">
          <span
            class="sf-chip"
            :style="row.onlineStatus === 1 ? 'border-color: rgba(51,209,122,0.25)' : 'border-color: rgba(255,255,255,0.12)'"
            >{{ row.onlineStatus === 1 ? "在线" : "离线" }}</span
          >
        </template>
      </el-table-column>
      <el-table-column label="操作" width="210" fixed="right">
        <template #default="{ row }">
          <el-button size="small" @click="openTrend(row.deviceCode)">趋势</el-button>
          <el-button size="small" @click="openEdit(row)">编辑</el-button>
          <el-button size="small" type="danger" plain @click="onDelete(row.id)">删除</el-button>
        </template>
      </el-table-column>
      </el-table>

      <div style="display: flex; justify-content: flex-end; margin-top: 12px">
        <el-pagination
          background
          layout="prev, pager, next, sizes, total"
          :total="total"
          v-model:current-page="page"
          v-model:page-size="pageSize"
          @change="load"
        />
      </div>
    </div>

    <el-dialog v-model="dialogVisible" :title="editing?.id ? '编辑设备' : '新增设备'" width="720px">
      <el-form label-width="110px">
        <div class="sf-form-grid">
          <el-form-item label="企业编码"><el-input v-model="form.companyCode" /></el-form-item>
          <el-form-item label="设备编码" required><el-input v-model="form.deviceCode" /></el-form-item>
          <el-form-item label="设备名称" required><el-input v-model="form.deviceName" /></el-form-item>
          <el-form-item label="设备类型" required>
            <el-select v-model="form.deviceType" style="width: 100%" placeholder="选择类型">
              <el-option v-for="t in deviceTypeOptions" :key="t.value" :label="t.label" :value="t.value" />
            </el-select>
          </el-form-item>
          <el-form-item label="单位"><el-input v-model="form.unit" /></el-form-item>
          <el-form-item label="下限"><el-input-number v-model="form.lowerLimit" style="width: 100%" /></el-form-item>
          <el-form-item label="上限"><el-input-number v-model="form.upperLimit" style="width: 100%" /></el-form-item>
          <el-form-item label="位置"><el-input v-model="form.locationName" /></el-form-item>
          <el-form-item label="仓库编码"><el-input v-model="form.storeNum" /></el-form-item>
          <el-form-item label="库房编码"><el-input v-model="form.storeroomNum" /></el-form-item>
          <el-form-item label="IP地址"><el-input v-model="form.ipAddress" /></el-form-item>
          <el-form-item label="账号"><el-input v-model="form.accessUsername" /></el-form-item>
          <el-form-item label="密码"><el-input v-model="form.accessPassword" show-password /></el-form-item>
          <el-form-item v-if="editing?.id" label="在线状态">
          <el-switch v-model="form.onlineStatus" :active-value="1" :inactive-value="0" />
          </el-form-item>
        </div>
      </el-form>
      <template #footer>
        <el-button @click="dialogVisible = false">取消</el-button>
        <el-button type="primary" :loading="saving" @click="onSave">保存</el-button>
      </template>
    </el-dialog>

    <el-dialog v-model="trendVisible" title="设备趋势（小时聚合）" width="820px">
      <div ref="chartEl" style="height: 360px; width: 100%"></div>
      <template #footer>
        <el-button @click="trendVisible = false">关闭</el-button>
      </template>
    </el-dialog>
  </div>
</template>

<script setup lang="ts">
import { nextTick, reactive, ref, watch } from "vue";
import { ElMessage } from "element-plus";
import { http } from "@/api/http";
import * as echarts from "echarts";

type DeviceVO = {
  id: number;
  companyCode: string | null;
  deviceCode: string;
  deviceName: string;
  deviceType: number;
  unit: string | null;
  lowerLimit: number | null;
  upperLimit: number | null;
  locationName: string | null;
  storeNum: string | null;
  storeroomNum: string | null;
  ipAddress: string | null;
  accessUsername: string | null;
  accessPassword: string | null;
  onlineStatus: number;
};

type PageResponse<T> = { list: T[]; page: number; pageSize: number; total: number };

const loading = ref(false);
const list = ref<DeviceVO[]>([]);
const total = ref(0);
const page = ref(1);
const pageSize = ref(20);
const deviceTypeFilter = ref<number | null>(null);

const deviceTypeOptions = [
  { label: "视频", value: 1 },
  { label: "红外", value: 2 },
  { label: "温度", value: 3 },
  { label: "湿度", value: 4 },
  { label: "液位", value: 5 }
];

function deviceTypeLabel(value?: number | null) {
  const found = deviceTypeOptions.find((item) => item.value === value);
  return found ? found.label : "未知";
}

const dialogVisible = ref(false);
const saving = ref(false);
const editing = ref<DeviceVO | null>(null);
const form = reactive({
  companyCode: "",
  deviceCode: "",
  deviceName: "",
  deviceType: 3,
  unit: "",
  lowerLimit: null as number | null,
  upperLimit: null as number | null,
  locationName: "",
  storeNum: "",
  storeroomNum: "",
  ipAddress: "",
  accessUsername: "",
  accessPassword: "",
  onlineStatus: 0
});

const trendVisible = ref(false);
const chartEl = ref<HTMLDivElement | null>(null);
let chart: echarts.ECharts | null = null;

async function load() {
  loading.value = true;
  try {
    const data = await http.get<PageResponse<DeviceVO>>(
      `/api/v1/devices?page=${page.value}&pageSize=${pageSize.value}${deviceTypeFilter.value ? `&deviceType=${deviceTypeFilter.value}` : ""}`
    );
    list.value = data.list;
    total.value = data.total;
  } catch (e: any) {
    ElMessage.error(e?.message || "加载失败");
  } finally {
    loading.value = false;
  }
}

function openCreate() {
  editing.value = null;
  Object.assign(form, {
    companyCode: "",
    deviceCode: "",
    deviceName: "",
    deviceType: 3,
    unit: "",
    lowerLimit: null,
    upperLimit: null,
    locationName: "",
    storeNum: "",
    storeroomNum: "",
    ipAddress: "",
    accessUsername: "",
    accessPassword: "",
    onlineStatus: 0
  });
  dialogVisible.value = true;
}

function openEdit(row: DeviceVO) {
  editing.value = row;
  Object.assign(form, {
    companyCode: row.companyCode || "",
    deviceCode: row.deviceCode,
    deviceName: row.deviceName,
    deviceType: row.deviceType,
    unit: row.unit || "",
    lowerLimit: row.lowerLimit,
    upperLimit: row.upperLimit,
    locationName: row.locationName || "",
    storeNum: row.storeNum || "",
    storeroomNum: row.storeroomNum || "",
    ipAddress: row.ipAddress || "",
    accessUsername: row.accessUsername || "",
    accessPassword: row.accessPassword || "",
    onlineStatus: row.onlineStatus
  });
  dialogVisible.value = true;
}

async function onSave() {
  if (!form.deviceCode || !form.deviceName) {
    ElMessage.warning("请填写设备编码与设备名称");
    return;
  }
  saving.value = true;
  try {
    if (!editing.value?.id) {
      await http.post<number>("/api/v1/devices", { ...form });
    } else {
      await http.put<void>("/api/v1/devices", { id: editing.value.id, ...form });
    }
    ElMessage.success("保存成功");
    dialogVisible.value = false;
    await load();
  } catch (e: any) {
    ElMessage.error(e?.message || "保存失败");
  } finally {
    saving.value = false;
  }
}

async function onDelete(id: number) {
  try {
    await http.del<void>(`/api/v1/devices/${id}`);
    ElMessage.success("删除成功");
    await load();
  } catch (e: any) {
    ElMessage.error(e?.message || "删除失败");
  }
}

async function openTrend(deviceCode: string) {
  trendVisible.value = true;
  await nextTick();
  try {
    const data = await http.get<{ deviceCode: string; hourStart: number; avgValue: number; minValue: number; maxValue: number; samples: number }[]>(
      `/api/v1/readings/hourly?deviceCode=${encodeURIComponent(deviceCode)}&hours=24`
    );
    if (!chartEl.value) return;
    chart?.dispose();
    chart = echarts.init(chartEl.value);
    chart.setOption({
      backgroundColor: "transparent",
      grid: { left: 40, right: 18, top: 22, bottom: 34 },
      tooltip: { trigger: "axis" },
      xAxis: {
        type: "category",
        data: data.map((x) => new Date(x.hourStart).getHours().toString().padStart(2, "0") + ":00"),
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
          name: "平均值",
          type: "line",
          smooth: true,
          data: data.map((x) => x.avgValue),
          lineStyle: { width: 3, color: "rgba(61,214,198,0.95)" },
          symbol: "circle",
          symbolSize: 7
        }
      ]
    });
  } catch (e: any) {
    ElMessage.error(e?.message || "加载趋势失败");
  }
}

watch([page, pageSize], () => load());
watch(deviceTypeFilter, () => {
  page.value = 1;
  load();
});
load();
</script>
