<template>
  <div class="sf-page">
    <div class="sf-page-head">
      <div>
        <h2 class="sf-page-title">硬件数据（上报日志）</h2>
        <div class="sf-page-sub">HTTP 通道上报记录与解析状态</div>
      </div>
      <div class="sf-page-actions">
        <el-button @click="load">查询</el-button>
      </div>
    </div>

    <div class="sf-section" style="margin-bottom: 14px">
      <div class="sf-filter">
        <el-select v-model="filters.channel" placeholder="通道" clearable style="width: 120px">
          <el-option label="HTTP" value="HTTP" />
        </el-select>
        <el-select v-model="filters.messageType" placeholder="类型" clearable style="width: 180px">
          <el-option label="device-reading（实时值）" value="device-reading" />
          <el-option label="alarm（告警）" value="alarm" />
          <el-option label="person-inout（人员出入）" value="person-inout" />
          <el-option label="car-inout（车辆出入）" value="car-inout" />
          <el-option label="server-heartbeat（服务器心跳）" value="server-heartbeat" />
        </el-select>
        <el-select v-model="filters.ok" placeholder="结果" clearable style="width: 120px">
          <el-option label="成功" :value="1" />
          <el-option label="失败" :value="0" />
        </el-select>
        <el-input v-model="filters.companyCode" placeholder="企业编码" style="width: 140px" clearable />
        <el-input v-model="filters.apiKey" placeholder="apiKey" style="width: 150px" clearable />
        <el-input v-model="filters.topicLike" placeholder="topic/path 包含" style="width: 180px" clearable />
        <el-button @click="load">查询</el-button>
      </div>
    </div>

    <div class="sf-kpi sf-stagger" style="margin-bottom: 14px">
      <div class="sf-card sf-kpi-item" style="--i: 40ms">
        <div class="sf-kpi-title">本页上报</div>
        <div class="sf-kpi-value">{{ list.length }}</div>
      </div>
      <div class="sf-card sf-kpi-item" style="--i: 80ms">
        <div class="sf-kpi-title">解析成功</div>
        <div class="sf-kpi-value" style="color: rgba(39, 183, 167, 0.95)">{{ successCount }}</div>
      </div>
      <div class="sf-card sf-kpi-item" style="--i: 120ms">
        <div class="sf-kpi-title">解析失败</div>
        <div class="sf-kpi-value" style="color: rgba(255, 107, 107, 0.95)">{{ failCount }}</div>
      </div>
      <div class="sf-card sf-kpi-item" style="--i: 160ms">
        <div class="sf-kpi-title">失败率</div>
        <div class="sf-kpi-value">{{ failRate }}%</div>
      </div>
    </div>

    <div class="sf-card sf-table-card">
      <el-table :data="list" style="width: 100%" v-loading="loading">
      <el-table-column prop="id" label="ID" width="90" />
      <el-table-column prop="receiveTimeMs" label="时间" width="170">
        <template #default="{ row }">
          <span class="sf-muted">{{ formatTime(row.receiveTimeMs) }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="ingestChannel" label="通道" width="90" />
      <el-table-column prop="messageType" label="类型" width="160">
        <template #default="{ row }">
          <span class="sf-chip">{{ row.messageType }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="companyCode" label="企业" width="110">
        <template #default="{ row }">
          <span class="sf-chip">{{ row.companyCode || "-" }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="apiKey" label="apiKey" min-width="140" />
      <el-table-column prop="topic" label="topic/path" min-width="200" show-overflow-tooltip />
      <el-table-column prop="parsedOk" label="结果" width="90">
        <template #default="{ row }">
          <span class="sf-chip" :style="row.parsedOk === 1 ? 'border-color: rgba(51,209,122,0.25)' : 'border-color: rgba(255,107,107,0.22)'">
            {{ row.parsedOk === 1 ? "成功" : "失败" }}
          </span>
        </template>
      </el-table-column>
      <el-table-column prop="errorMessage" label="失败原因" min-width="180" show-overflow-tooltip />
      <el-table-column label="操作" width="120" fixed="right">
        <template #default="{ row }">
          <el-button size="small" @click="openPayload(row)">查看</el-button>
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

    <el-dialog v-model="payloadVisible" title="原始上报数据" width="860px">
      <div style="display: grid; gap: 10px">
        <div class="sf-muted">提示：HTTP 会保存 body JSON。</div>
        <el-input v-model="payloadText" type="textarea" :rows="16" readonly />
      </div>
      <template #footer>
        <el-button @click="payloadVisible = false">关闭</el-button>
        <el-button type="primary" @click="copyPayload">复制</el-button>
      </template>
    </el-dialog>
  </div>
</template>

<script setup lang="ts">
import { computed, reactive, ref, watch } from "vue";
import { ElMessage } from "element-plus";
import { http } from "@/api/http";

type PageResponse<T> = { list: T[]; page: number; pageSize: number; total: number };

type HardwareIngestLog = {
  id: number;
  ingestChannel: string;
  topic: string | null;
  messageType: string;
  apiKey: string | null;
  companyCode: string | null;
  payload: string;
  parsedOk: number;
  errorMessage: string | null;
  receiveTimeMs: number;
};

const filters = reactive({
  channel: "" as "" | "HTTP",
  messageType: "" as "" | "device-reading" | "alarm" | "person-inout" | "car-inout" | "server-heartbeat",
  ok: null as null | 0 | 1,
  apiKey: "",
  companyCode: "",
  topicLike: ""
});

const loading = ref(false);
const list = ref<HardwareIngestLog[]>([]);
const total = ref(0);
const page = ref(1);
const pageSize = ref(20);

const payloadVisible = ref(false);
const payloadText = ref("");

const successCount = computed(() => list.value.filter((item) => item.parsedOk === 1).length);
const failCount = computed(() => list.value.filter((item) => item.parsedOk !== 1).length);
const failRate = computed(() => (list.value.length ? Math.round((failCount.value / list.value.length) * 100) : 0));

function formatTime(ms?: number) {
  if (!ms) return "-";
  const d = new Date(ms);
  const pad = (n: number) => String(n).padStart(2, "0");
  return `${d.getFullYear()}-${pad(d.getMonth() + 1)}-${pad(d.getDate())} ${pad(d.getHours())}:${pad(d.getMinutes())}:${pad(d.getSeconds())}`;
}

function buildQuery() {
  const params = new URLSearchParams();
  params.set("page", String(page.value));
  params.set("pageSize", String(pageSize.value));
  if (filters.channel) params.set("channel", filters.channel);
  if (filters.messageType) params.set("messageType", filters.messageType);
  if (filters.ok !== null) params.set("ok", String(filters.ok));
  if (filters.apiKey) params.set("apiKey", filters.apiKey);
  if (filters.companyCode) params.set("companyCode", filters.companyCode);
  if (filters.topicLike) params.set("topicLike", filters.topicLike);
  return params.toString();
}

async function load() {
  loading.value = true;
  try {
    const data = await http.get<PageResponse<HardwareIngestLog>>(`/api/v1/hardware/ingest-logs?${buildQuery()}`);
    list.value = data.list;
    total.value = data.total;
  } catch (e: any) {
    ElMessage.error(e?.message || "加载失败");
  } finally {
    loading.value = false;
  }
}

function openPayload(row: HardwareIngestLog) {
  payloadText.value = row.payload || "";
  payloadVisible.value = true;
}

async function copyPayload() {
  try {
    await navigator.clipboard.writeText(payloadText.value || "");
    ElMessage.success("已复制");
  } catch {
    ElMessage.error("复制失败（浏览器权限限制）");
  }
}

watch([page, pageSize], () => load());
watch(
  () => ({ ...filters }),
  () => {
    page.value = 1;
  }
);

load();
</script>
