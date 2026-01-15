<template>
  <div class="sf-page">
    <div class="sf-page-head">
      <div>
        <h2 class="sf-page-title">温度信息</h2>
        <div class="sf-page-sub">温度设备与阈值配置</div>
      </div>
      <div class="sf-page-actions">
        <el-select v-model="onlineFilter" placeholder="在线状态" style="width: 140px" clearable>
          <el-option label="在线" :value="1" />
          <el-option label="离线" :value="0" />
        </el-select>
        <el-button @click="load">刷新</el-button>
      </div>
    </div>

    <div class="sf-kpi sf-stagger" style="margin-bottom: 14px">
      <div class="sf-card sf-kpi-item" style="--i: 40ms">
        <div class="sf-kpi-title">本页设备</div>
        <div class="sf-kpi-value">{{ list.length }}</div>
      </div>
      <div class="sf-card sf-kpi-item" style="--i: 80ms">
        <div class="sf-kpi-title">在线</div>
        <div class="sf-kpi-value" style="color: rgba(39, 183, 167, 0.95)">{{ onlineCount }}</div>
      </div>
      <div class="sf-card sf-kpi-item" style="--i: 120ms">
        <div class="sf-kpi-title">离线</div>
        <div class="sf-kpi-value" style="color: rgba(255, 107, 107, 0.95)">{{ offlineCount }}</div>
      </div>
      <div class="sf-card sf-kpi-item" style="--i: 160ms">
        <div class="sf-kpi-title">总记录数</div>
        <div class="sf-kpi-value">{{ total }}</div>
      </div>
    </div>

    <div class="sf-card sf-table-card">
      <el-table :data="list" v-loading="loading" style="width: 100%">
        <el-table-column prop="deviceCode" label="设备编码" min-width="140" />
        <el-table-column prop="deviceName" label="设备名称" min-width="180" />
        <el-table-column prop="locationName" label="位置" min-width="160" />
        <el-table-column prop="storeNum" label="仓库编码" min-width="140" />
        <el-table-column prop="storeroomNum" label="库房编码" min-width="140" />
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
            <span class="sf-chip" :style="row.onlineStatus === 1 ? 'border-color: rgba(51,209,122,0.25)' : ''">
              {{ row.onlineStatus === 1 ? "在线" : "离线" }}
            </span>
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
  </div>
</template>

<script setup lang="ts">
import { computed, ref, watch } from "vue";
import { ElMessage } from "element-plus";
import { http } from "@/api/http";

type DeviceVO = {
  id: number;
  deviceCode: string;
  deviceName: string;
  deviceType: number;
  unit: string | null;
  lowerLimit: number | null;
  upperLimit: number | null;
  locationName: string | null;
  storeNum: string | null;
  storeroomNum: string | null;
  onlineStatus: number;
};

type PageResponse<T> = { list: T[]; page: number; pageSize: number; total: number };

const list = ref<DeviceVO[]>([]);
const loading = ref(false);
const page = ref(1);
const pageSize = ref(20);
const total = ref(0);
const onlineFilter = ref<number | null>(null);

const onlineCount = computed(() => list.value.filter((item) => item.onlineStatus === 1).length);
const offlineCount = computed(() => list.value.filter((item) => item.onlineStatus !== 1).length);

async function load() {
  loading.value = true;
  try {
    const data = await http.get<PageResponse<DeviceVO>>(`/api/v1/devices?page=${page.value}&pageSize=${pageSize.value}&deviceType=3`);
    const incoming = data.list;
    list.value = onlineFilter.value === null ? incoming : incoming.filter((item) => item.onlineStatus === onlineFilter.value);
    total.value = data.total;
  } catch (e: any) {
    ElMessage.error(e?.message || "加载失败");
  } finally {
    loading.value = false;
  }
}

watch([page, pageSize], () => load());
watch(onlineFilter, () => {
  page.value = 1;
  load();
});
load();
</script>
