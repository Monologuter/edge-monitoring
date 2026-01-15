<template>
  <div class="sf-page">
    <div class="sf-page-head">
      <div>
        <h2 class="sf-page-title">预警推送记录</h2>
        <div class="sf-page-sub">联动推送状态与错误追踪</div>
      </div>
      <div class="sf-page-actions">
        <el-select v-model="status" placeholder="状态" style="width: 140px" clearable>
          <el-option label="PENDING" value="PENDING" />
          <el-option label="SENT" value="SENT" />
          <el-option label="FAILED" value="FAILED" />
        </el-select>
        <el-button @click="load">刷新</el-button>
      </div>
    </div>

    <div class="sf-kpi sf-stagger" style="margin-bottom: 14px">
      <div class="sf-card sf-kpi-item" style="--i: 40ms">
        <div class="sf-kpi-title">本页记录</div>
        <div class="sf-kpi-value">{{ list.length }}</div>
      </div>
      <div class="sf-card sf-kpi-item" style="--i: 80ms">
        <div class="sf-kpi-title">待推送</div>
        <div class="sf-kpi-value">{{ pendingCount }}</div>
      </div>
      <div class="sf-card sf-kpi-item" style="--i: 120ms">
        <div class="sf-kpi-title">已推送</div>
        <div class="sf-kpi-value" style="color: rgba(39, 183, 167, 0.95)">{{ sentCount }}</div>
      </div>
      <div class="sf-card sf-kpi-item" style="--i: 160ms">
        <div class="sf-kpi-title">失败</div>
        <div class="sf-kpi-value" style="color: rgba(255, 107, 107, 0.95)">{{ failedCount }}</div>
      </div>
    </div>

    <div class="sf-card sf-table-card">
      <el-table :data="list" v-loading="loading" style="width: 100%">
        <el-table-column prop="id" label="ID" width="90" />
        <el-table-column prop="alarmId" label="告警ID" width="110" />
        <el-table-column prop="linkageType" label="联动类型" width="140" />
        <el-table-column prop="target" label="目标" min-width="180" />
        <el-table-column prop="payload" label="内容" min-width="220" show-overflow-tooltip />
        <el-table-column prop="status" label="状态" width="110">
          <template #default="{ row }">
            <span
              class="sf-chip"
              :style="
                row.status === 'FAILED'
                  ? 'border-color: rgba(255,107,107,0.25)'
                  : row.status === 'SENT'
                    ? 'border-color: rgba(39,183,167,0.25)'
                    : ''
              "
            >
              {{ row.status }}
            </span>
          </template>
        </el-table-column>
        <el-table-column prop="lastError" label="错误信息" min-width="220" show-overflow-tooltip />
        <el-table-column prop="createdAt" label="创建时间" min-width="170">
          <template #default="{ row }">{{ row.createdAt ? fmt(row.createdAt) : "—" }}</template>
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

type LinkageEvent = {
  id: number;
  alarmId: number | null;
  linkageType: string;
  target: string;
  payload: string | null;
  status: string;
  lastError: string | null;
  createdAt: number | null;
  updatedAt: number | null;
};

type PageResponse<T> = { list: T[]; page: number; pageSize: number; total: number };

const list = ref<LinkageEvent[]>([]);
const loading = ref(false);
const page = ref(1);
const pageSize = ref(20);
const total = ref(0);
const status = ref<string | undefined>(undefined);

const pendingCount = computed(() => list.value.filter((item) => item.status === "PENDING").length);
const sentCount = computed(() => list.value.filter((item) => item.status === "SENT").length);
const failedCount = computed(() => list.value.filter((item) => item.status === "FAILED").length);

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
    const data = await http.get<PageResponse<LinkageEvent>>(
      `/api/v1/linkage/events?page=${page.value}&pageSize=${pageSize.value}${status.value ? `&status=${status.value}` : ""}`
    );
    list.value = data.list;
    total.value = data.total;
  } catch (e: any) {
    ElMessage.error(e?.message || "加载失败");
  } finally {
    loading.value = false;
  }
}

watch([page, pageSize, status], () => load());
load();
</script>
