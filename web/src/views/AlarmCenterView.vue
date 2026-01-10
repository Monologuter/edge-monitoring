<template>
  <div>
    <div style="display: flex; align-items: baseline; justify-content: space-between; margin-bottom: 14px">
      <h2 class="sf-title">告警中心</h2>
      <div style="display: flex; gap: 10px; align-items: center">
        <el-select v-model="status" placeholder="状态" style="width: 140px" clearable>
          <el-option label="ACTIVE" value="ACTIVE" />
          <el-option label="CLEARED" value="CLEARED" />
        </el-select>
        <el-button type="primary" @click="load">刷新</el-button>
      </div>
    </div>

    <el-table :data="list" style="width: 100%" v-loading="loading">
      <el-table-column prop="id" label="ID" width="90" />
      <el-table-column prop="alarmType" label="类型" min-width="140" />
      <el-table-column prop="alarmStatus" label="状态" width="110">
        <template #default="{ row }">
          <span
            class="sf-chip"
            :style="
              row.alarmStatus === 'ACTIVE'
                ? 'border-color: rgba(255,77,79,0.28)'
                : 'border-color: rgba(51,209,122,0.25)'
            "
            >{{ row.alarmStatus }}</span
          >
        </template>
      </el-table-column>
      <el-table-column prop="workflowStatus" label="流程" width="130" />
      <el-table-column prop="riskLevel" label="等级" width="110" />
      <el-table-column prop="deviceCode" label="设备编码" min-width="130" />
      <el-table-column prop="warningTime" label="告警时间" min-width="170">
        <template #default="{ row }">{{ fmt(row.warningTime) }}</template>
      </el-table-column>
      <el-table-column prop="archivedTime" label="归档时间" min-width="170">
        <template #default="{ row }">{{ row.archivedTime ? fmt(row.archivedTime) : "—" }}</template>
      </el-table-column>
      <el-table-column prop="handler" label="处理人" width="120" />
      <el-table-column prop="remark" label="备注" min-width="220" />
      <el-table-column label="操作" width="160" fixed="right">
        <template #default="{ row }">
          <el-button size="small" @click="handle(row.id, 'HANDLE')">处理</el-button>
          <el-button size="small" type="danger" plain @click="handle(row.id, 'CLEAR')">消警</el-button>
          <el-button size="small" type="warning" plain @click="handle(row.id, 'ARCHIVE')">归档</el-button>
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
</template>

<script setup lang="ts">
import { onMounted, onUnmounted, ref, watch } from "vue";
import { ElMessage, ElMessageBox } from "element-plus";
import { http } from "@/api/http";
import { connectStomp } from "@/realtime/stomp";

type AlarmVO = {
  id: number;
  alarmType: string;
  alarmStatus: string;
  workflowStatus: string;
  riskLevel: string;
  deviceCode: string | null;
  warningTime: number;
  clearTime: number | null;
  archivedTime: number | null;
  handler: string | null;
  remark: string | null;
};

type PageResponse<T> = { list: T[]; page: number; pageSize: number; total: number };

const loading = ref(false);
const list = ref<AlarmVO[]>([]);
const total = ref(0);
const page = ref(1);
const pageSize = ref(20);
const status = ref<string | undefined>(undefined);
let stomp: ReturnType<typeof connectStomp> | null = null;
let sub: { unsubscribe: () => void } | null = null;

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
    const data = await http.get<PageResponse<AlarmVO>>(
      `/api/v1/alarms?page=${page.value}&pageSize=${pageSize.value}${status.value ? `&status=${status.value}` : ""}`
    );
    list.value = data.list;
    total.value = data.total;
  } catch (e: any) {
    ElMessage.error(e?.message || "加载失败");
  } finally {
    loading.value = false;
  }
}

async function handle(id: number, action: "HANDLE" | "CLEAR" | "ARCHIVE") {
  const remark = await ElMessageBox.prompt(action === "HANDLE" ? "请输入处理说明" : "请输入消警说明", "操作确认", {
    confirmButtonText: "确定",
    cancelButtonText: "取消",
    inputPlaceholder: "可留空"
  }).catch(() => null);
  if (remark === null) return;
  try {
    await http.post<void>("/api/v1/alarms/handle", { alarmId: id, action, remark: remark.value || "" });
    ElMessage.success("操作成功");
    await load();
  } catch (e: any) {
    ElMessage.error(e?.message || "操作失败");
  }
}

watch([page, pageSize, status], () => load());

onMounted(() => {
  load();
  stomp = connectStomp(() => {});
  stomp.activate();
  sub = stomp.subscribeJson<AlarmVO>("/topic/alarms", (a) => {
    // 只在当前筛选下更新（简单做法：直接刷新）
    void load();
  });
});

onUnmounted(() => {
  sub?.unsubscribe();
  void stomp?.deactivate();
});
</script>
