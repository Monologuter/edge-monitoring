<template>
  <div class="sf-page">
    <div class="sf-page-head">
      <div>
        <h2 class="sf-page-title">审计日志</h2>
        <div class="sf-page-sub">系统操作、请求与行为追踪</div>
      </div>
      <div class="sf-page-actions">
        <el-button @click="load">刷新</el-button>
      </div>
    </div>

    <div class="sf-kpi sf-stagger" style="margin-bottom: 14px">
      <div class="sf-card sf-kpi-item" style="--i: 40ms">
        <div class="sf-kpi-title">本页日志数</div>
        <div class="sf-kpi-value">{{ list.length }}</div>
      </div>
      <div class="sf-card sf-kpi-item" style="--i: 80ms">
        <div class="sf-kpi-title">异常响应</div>
        <div class="sf-kpi-value" style="color: rgba(255, 107, 107, 0.95)">{{ errorCount }}</div>
      </div>
      <div class="sf-card sf-kpi-item" style="--i: 120ms">
        <div class="sf-kpi-title">平均耗时</div>
        <div class="sf-kpi-value">{{ avgCost }} ms</div>
      </div>
      <div class="sf-card sf-kpi-item" style="--i: 160ms">
        <div class="sf-kpi-title">总记录数</div>
        <div class="sf-kpi-value">{{ total }}</div>
      </div>
    </div>

    <div class="sf-card sf-table-card">
      <el-table :data="list" v-loading="loading" style="width: 100%">
      <el-table-column prop="id" label="ID" width="90" />
      <el-table-column prop="requestId" label="requestId" min-width="160" />
      <el-table-column prop="username" label="用户" width="120" />
      <el-table-column prop="method" label="方法" width="90" />
      <el-table-column prop="uri" label="URI" min-width="240" />
      <el-table-column prop="ip" label="IP" width="140" />
      <el-table-column prop="action" label="动作" min-width="160" />
      <el-table-column prop="responseCode" label="code" width="90">
        <template #default="{ row }">
          <span class="sf-chip" :style="row.responseCode && row.responseCode >= 400 ? 'border-color: rgba(255,107,107,0.25)' : ''">
            {{ row.responseCode ?? "-" }}
          </span>
        </template>
      </el-table-column>
      <el-table-column prop="costMs" label="耗时" width="100">
        <template #default="{ row }">
          <span class="sf-chip" :style="row.costMs && row.costMs >= 1000 ? 'border-color: rgba(243,178,90,0.25)' : ''">
            {{ row.costMs ?? "-" }}
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

type PageResponse<T> = { list: T[]; page: number; pageSize: number; total: number };
type AuditLog = {
  id: number;
  requestId: string | null;
  username: string | null;
  uri: string;
  method: string;
  ip: string | null;
  action: string | null;
  responseCode: number | null;
  costMs: number | null;
};

const loading = ref(false);
const list = ref<AuditLog[]>([]);
const total = ref(0);
const page = ref(1);
const pageSize = ref(20);

const errorCount = computed(() => list.value.filter((item) => (item.responseCode ?? 0) >= 400).length);
const avgCost = computed(() => {
  if (!list.value.length) return 0;
  const sum = list.value.reduce((acc, cur) => acc + (cur.costMs || 0), 0);
  return Math.round(sum / list.value.length);
});

async function load() {
  loading.value = true;
  try {
    const data = await http.get<PageResponse<AuditLog>>(`/api/v1/audit-logs?page=${page.value}&pageSize=${pageSize.value}`);
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
