<template>
  <div class="sf-page">
    <div class="sf-page-head">
      <div>
        <h2 class="sf-page-title">服务器信息</h2>
        <div class="sf-page-sub">采集服务器与心跳状态</div>
      </div>
      <div class="sf-page-actions">
        <el-button type="primary" @click="load">刷新</el-button>
      </div>
    </div>

    <div class="sf-kpi sf-stagger" style="margin-bottom: 14px">
      <div class="sf-card sf-kpi-item" style="--i: 40ms">
        <div class="sf-kpi-title">本页服务器</div>
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
      <el-table-column prop="computerName" label="服务器名称" min-width="180" />
      <el-table-column prop="ip" label="服务器IP" min-width="160" />
      <el-table-column prop="onlineStatus" label="在线状态" width="110">
        <template #default="{ row }">
          <span class="sf-chip" :style="row.onlineStatus === 1 ? 'border-color: rgba(46,194,126,0.25)' : ''">
            {{ row.onlineStatus === 1 ? "在线" : "离线" }}
          </span>
        </template>
      </el-table-column>
      <el-table-column prop="lastHeartbeatTime" label="最近心跳" min-width="170">
        <template #default="{ row }">{{ row.lastHeartbeatTime ? fmt(row.lastHeartbeatTime) : "-" }}</template>
      </el-table-column>
      <el-table-column prop="createdTime" label="创建日期" min-width="170">
        <template #default="{ row }">{{ row.createdTime ? fmt(row.createdTime) : "-" }}</template>
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

type PageResponse<T> = { list: T[]; page: number; pageSize: number; total: number };

type ServerMachineVO = {
  id: number;
  companyCode: string;
  computerName: string;
  ip: string;
  originalId: string | null;
  onlineStatus: number;
  lastHeartbeatTime: number | null;
  createdTime: number | null;
};

const list = ref<ServerMachineVO[]>([]);
const loading = ref(false);
const page = ref(1);
const pageSize = ref(20);
const total = ref(0);

const onlineCount = computed(() => list.value.filter((item) => item.onlineStatus === 1).length);
const offlineCount = computed(() => list.value.filter((item) => item.onlineStatus !== 1).length);

function fmt(ts: number) {
  const d = new Date(ts);
  const pad = (n: number) => String(n).padStart(2, "0");
  return `${d.getFullYear()}-${pad(d.getMonth() + 1)}-${pad(d.getDate())} ${pad(d.getHours())}:${pad(d.getMinutes())}:${pad(
    d.getSeconds()
  )}`;
}

async function load() {
  loading.value = true;
  try {
    const data = await http.get<PageResponse<ServerMachineVO>>(`/api/v1/servers?page=${page.value}&pageSize=${pageSize.value}`);
    list.value = data.list;
    total.value = data.total;
  } catch (e: any) {
    ElMessage.error(e?.message || "加载失败");
  } finally {
    loading.value = false;
  }
}

watch([page, pageSize], () => load());

load();
</script>
