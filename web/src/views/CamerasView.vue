<template>
  <div class="sf-page">
    <div class="sf-page-head">
      <div>
        <h2 class="sf-page-title">视频接入</h2>
        <div class="sf-page-sub">摄像头接入与流地址管理</div>
      </div>
      <div class="sf-page-actions">
        <el-input v-model="keyword" placeholder="编码/名称/企业编码" style="width: 240px" clearable />
        <el-button type="primary" @click="openCreate">新增摄像头</el-button>
        <el-button @click="load">刷新</el-button>
      </div>
    </div>

    <div class="sf-card sf-table-card">
      <el-table :data="list" v-loading="loading" style="width: 100%">
      <el-table-column prop="cameraCode" label="编码" min-width="140" />
      <el-table-column prop="cameraName" label="名称" min-width="180" />
      <el-table-column prop="companyCode" label="企业编码" min-width="140" />
      <el-table-column prop="locationName" label="位置" min-width="160" />
      <el-table-column prop="storeNum" label="仓库编码" min-width="140" />
      <el-table-column prop="storeroomNum" label="库房编码" min-width="140" />
      <el-table-column prop="streamUrl" label="流地址" min-width="260" />
      <el-table-column prop="enabled" label="启用" width="90">
        <template #default="{ row }">
          <span class="sf-chip" :style="row.enabled === 1 ? 'border-color: rgba(51,209,122,0.25)' : ''">
            {{ row.enabled === 1 ? "启用" : "停用" }}
          </span>
        </template>
      </el-table-column>
      <el-table-column label="操作" width="180" fixed="right">
        <template #default="{ row }">
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

    <el-dialog v-model="dialogVisible" :title="editing?.id ? '编辑摄像头' : '新增摄像头'" width="720px">
      <el-form label-width="100px">
        <div class="sf-form-grid">
          <el-form-item label="企业编码"><el-input v-model="form.companyCode" /></el-form-item>
          <el-form-item label="摄像头编码" required><el-input v-model="form.cameraCode" /></el-form-item>
          <el-form-item label="摄像头名称" required><el-input v-model="form.cameraName" /></el-form-item>
          <el-form-item label="流地址" required class="sf-form-full"><el-input v-model="form.streamUrl" /></el-form-item>
          <el-form-item label="位置"><el-input v-model="form.locationName" /></el-form-item>
          <el-form-item label="仓库编码"><el-input v-model="form.storeNum" /></el-form-item>
          <el-form-item label="库房编码"><el-input v-model="form.storeroomNum" /></el-form-item>
          <el-form-item label="启用"><el-switch v-model="form.enabled" :active-value="1" :inactive-value="0" /></el-form-item>
        </div>
      </el-form>
      <template #footer>
        <el-button @click="dialogVisible = false">取消</el-button>
        <el-button type="primary" :loading="saving" @click="onSave">保存</el-button>
      </template>
    </el-dialog>
  </div>
</template>

<script setup lang="ts">
import { reactive, ref, watch } from "vue";
import { ElMessage, ElMessageBox } from "element-plus";
import { http } from "@/api/http";

type PageResponse<T> = { list: T[]; page: number; pageSize: number; total: number };
type CameraVO = {
  id: number;
  companyCode: string | null;
  cameraCode: string;
  cameraName: string;
  streamUrl: string;
  locationName: string | null;
  storeNum: string | null;
  storeroomNum: string | null;
  enabled: number;
};

const keyword = ref("");
const page = ref(1);
const pageSize = ref(20);
const total = ref(0);
const loading = ref(false);
const list = ref<CameraVO[]>([]);

const dialogVisible = ref(false);
const saving = ref(false);
const editing = ref<CameraVO | null>(null);

const form = reactive({
  companyCode: "",
  cameraCode: "",
  cameraName: "",
  streamUrl: "",
  locationName: "",
  storeNum: "",
  storeroomNum: "",
  enabled: 1
});

async function load() {
  loading.value = true;
  try {
    const data = await http.get<PageResponse<CameraVO>>(
      `/api/v1/cameras?page=${page.value}&pageSize=${pageSize.value}${keyword.value ? `&keyword=${encodeURIComponent(keyword.value)}` : ""}`
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
  Object.assign(form, { companyCode: "", cameraCode: "", cameraName: "", streamUrl: "", locationName: "", storeNum: "", storeroomNum: "", enabled: 1 });
  dialogVisible.value = true;
}

function openEdit(row: CameraVO) {
  editing.value = row;
  Object.assign(form, {
    companyCode: row.companyCode || "",
    cameraCode: row.cameraCode,
    cameraName: row.cameraName,
    streamUrl: row.streamUrl,
    locationName: row.locationName || "",
    storeNum: row.storeNum || "",
    storeroomNum: row.storeroomNum || "",
    enabled: row.enabled
  });
  dialogVisible.value = true;
}

async function onSave() {
  if (!form.cameraCode || !form.cameraName || !form.streamUrl) {
    ElMessage.warning("请填写必填项");
    return;
  }
  saving.value = true;
  try {
    if (!editing.value?.id) {
      await http.post<number>("/api/v1/cameras", { ...form });
    } else {
      await http.put<void>("/api/v1/cameras", { id: editing.value.id, ...form });
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
  const ok = await ElMessageBox.confirm("确认删除该摄像头？", "提示", { type: "warning" }).catch(() => null);
  if (!ok) return;
  try {
    await http.del<void>(`/api/v1/cameras/${id}`);
    ElMessage.success("删除成功");
    await load();
  } catch (e: any) {
    ElMessage.error(e?.message || "删除失败");
  }
}

watch([page, pageSize], () => load());
watch(keyword, () => {
  page.value = 1;
  load();
});
load();
</script>
