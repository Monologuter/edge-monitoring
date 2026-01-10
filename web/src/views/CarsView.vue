<template>
  <div>
    <div style="display: flex; align-items: baseline; justify-content: space-between; margin-bottom: 14px">
      <h2 class="sf-title">车辆管理</h2>
      <div style="display: flex; gap: 10px; align-items: center">
        <el-input v-model="keyword" placeholder="车牌/企业编码" style="width: 220px" clearable />
        <el-button type="primary" @click="openCreate">新增车辆</el-button>
        <el-button @click="loadCars">刷新</el-button>
      </div>
    </div>

    <el-tabs v-model="tab">
      <el-tab-pane label="车辆档案" name="cars">
        <el-table :data="cars" v-loading="loadingCars" style="width: 100%">
          <el-table-column prop="companyCode" label="企业编码" min-width="140" />
          <el-table-column prop="licensePlateNumber" label="车牌号" min-width="140" />
          <el-table-column prop="carType" label="类型" width="140" />
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
            :total="carsTotal"
            v-model:current-page="carsPage"
            v-model:page-size="carsPageSize"
            @change="loadCars"
          />
        </div>
      </el-tab-pane>

      <el-tab-pane label="出入记录" name="records">
        <el-table :data="records" v-loading="loadingRecords" style="width: 100%">
          <el-table-column prop="licensePlateNumber" label="车牌号" min-width="140" />
          <el-table-column prop="carType" label="类型" width="140" />
          <el-table-column prop="inOutState" label="进出" width="90" />
          <el-table-column prop="inOutTime" label="时间" min-width="180">
            <template #default="{ row }">{{ fmt(row.inOutTime) }}</template>
          </el-table-column>
          <el-table-column label="图片" width="110">
            <template #default="{ row }">
              <el-link v-if="row.imageFileId" :href="fileUrl(row.imageFileId)" target="_blank">查看</el-link>
              <span v-else class="sf-muted">—</span>
            </template>
          </el-table-column>
        </el-table>
        <div style="display: flex; justify-content: flex-end; margin-top: 12px">
          <el-pagination
            background
            layout="prev, pager, next, sizes, total"
            :total="recordsTotal"
            v-model:current-page="recordsPage"
            v-model:page-size="recordsPageSize"
            @change="loadRecords"
          />
        </div>
      </el-tab-pane>
    </el-tabs>

    <el-dialog v-model="dialogVisible" :title="editing?.id ? '编辑车辆' : '新增车辆'" width="560px">
      <el-form label-width="90px">
        <el-form-item label="企业编码" required>
          <el-input v-model="form.companyCode" />
        </el-form-item>
        <el-form-item label="车牌号" required>
          <el-input v-model="form.licensePlateNumber" />
        </el-form-item>
        <el-form-item label="类型">
          <el-input v-model="form.carType" placeholder="危化/其他" />
        </el-form-item>
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
import { fileUrl } from "@/api/http";

type PageResponse<T> = { list: T[]; page: number; pageSize: number; total: number };

type CarVO = { id: number; companyCode: string; licensePlateNumber: string; carType: string | null };
type CarInoutRecordVO = {
  id: number;
  licensePlateNumber: string;
  carType: string | null;
  inOutState: string;
  inOutTime: number;
  imageFileId: number | null;
};

const tab = ref<"cars" | "records">("cars");
const keyword = ref("");

const loadingCars = ref(false);
const cars = ref<CarVO[]>([]);
const carsTotal = ref(0);
const carsPage = ref(1);
const carsPageSize = ref(20);

const loadingRecords = ref(false);
const records = ref<CarInoutRecordVO[]>([]);
const recordsTotal = ref(0);
const recordsPage = ref(1);
const recordsPageSize = ref(20);

const dialogVisible = ref(false);
const saving = ref(false);
const editing = ref<CarVO | null>(null);

const form = reactive({ companyCode: "", licensePlateNumber: "", carType: "" });

function fmt(ts: number) {
  const d = new Date(ts);
  const pad = (n: number) => String(n).padStart(2, "0");
  return `${d.getFullYear()}-${pad(d.getMonth() + 1)}-${pad(d.getDate())} ${pad(d.getHours())}:${pad(d.getMinutes())}:${pad(d.getSeconds())}`;
}

async function loadCars() {
  loadingCars.value = true;
  try {
    const data = await http.get<PageResponse<CarVO>>(
      `/api/v1/cars?page=${carsPage.value}&pageSize=${carsPageSize.value}${keyword.value ? `&keyword=${encodeURIComponent(keyword.value)}` : ""}`
    );
    cars.value = data.list;
    carsTotal.value = data.total;
  } catch (e: any) {
    ElMessage.error(e?.message || "加载失败");
  } finally {
    loadingCars.value = false;
  }
}

async function loadRecords() {
  loadingRecords.value = true;
  try {
    const data = await http.get<PageResponse<CarInoutRecordVO>>(`/api/v1/car-inout?page=${recordsPage.value}&pageSize=${recordsPageSize.value}`);
    records.value = data.list;
    recordsTotal.value = data.total;
  } catch (e: any) {
    ElMessage.error(e?.message || "加载失败");
  } finally {
    loadingRecords.value = false;
  }
}

function openCreate() {
  editing.value = null;
  Object.assign(form, { companyCode: "", licensePlateNumber: "", carType: "" });
  dialogVisible.value = true;
}

function openEdit(row: CarVO) {
  editing.value = row;
  Object.assign(form, { companyCode: row.companyCode, licensePlateNumber: row.licensePlateNumber, carType: row.carType || "" });
  dialogVisible.value = true;
}

async function onSave() {
  if (!form.companyCode || !form.licensePlateNumber) {
    ElMessage.warning("请填写企业编码与车牌号");
    return;
  }
  saving.value = true;
  try {
    if (!editing.value?.id) {
      await http.post<number>("/api/v1/cars", { ...form });
    } else {
      await http.put<void>("/api/v1/cars", { id: editing.value.id, ...form });
    }
    ElMessage.success("保存成功");
    dialogVisible.value = false;
    await loadCars();
  } catch (e: any) {
    ElMessage.error(e?.message || "保存失败");
  } finally {
    saving.value = false;
  }
}

async function onDelete(id: number) {
  const ok = await ElMessageBox.confirm("确认删除该车辆？", "提示", { type: "warning" }).catch(() => null);
  if (!ok) return;
  try {
    await http.del<void>(`/api/v1/cars/${id}`);
    ElMessage.success("删除成功");
    await loadCars();
  } catch (e: any) {
    ElMessage.error(e?.message || "删除失败");
  }
}

watch([carsPage, carsPageSize], () => loadCars());
watch([recordsPage, recordsPageSize], () => loadRecords());
watch(keyword, () => {
  carsPage.value = 1;
  loadCars();
});
watch(tab, () => {
  if (tab.value === "records") loadRecords();
});

loadCars();
</script>
