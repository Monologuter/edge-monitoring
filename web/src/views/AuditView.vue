<template>
  <div>
    <div style="display: flex; align-items: baseline; justify-content: space-between; margin-bottom: 14px">
      <h2 class="sf-title">审计日志</h2>
      <el-button @click="load">刷新</el-button>
    </div>

    <el-table :data="list" v-loading="loading" style="width: 100%">
      <el-table-column prop="id" label="ID" width="90" />
      <el-table-column prop="requestId" label="requestId" min-width="160" />
      <el-table-column prop="username" label="用户" width="120" />
      <el-table-column prop="method" label="方法" width="90" />
      <el-table-column prop="uri" label="URI" min-width="240" />
      <el-table-column prop="ip" label="IP" width="140" />
      <el-table-column prop="action" label="动作" min-width="160" />
      <el-table-column prop="responseCode" label="code" width="90" />
      <el-table-column prop="costMs" label="耗时" width="90" />
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
</template>

<script setup lang="ts">
import { ref, watch } from "vue";
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

