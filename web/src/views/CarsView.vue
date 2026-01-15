<template>
  <div class="sf-page">
    <div class="sf-page-head">
      <div>
        <h2 class="sf-page-title">车辆管理</h2>
        <div class="sf-page-sub">车辆档案、证件与出入记录</div>
      </div>
      <div class="sf-page-actions">
        <el-input v-model="keyword" placeholder="车牌/企业编码" style="width: 220px" clearable />
        <el-button type="primary" @click="openCreate">新增车辆</el-button>
        <el-upload :show-file-list="false" :before-upload="importCars" accept=".csv">
          <el-button>导入</el-button>
        </el-upload>
        <el-button @click="loadCars">刷新</el-button>
      </div>
    </div>

    <div class="sf-card sf-table-card">
      <el-tabs v-model="tab">
      <el-tab-pane label="车辆档案" name="cars">
        <el-table :data="cars" v-loading="loadingCars" style="width: 100%">
          <el-table-column prop="companyCode" label="企业编码" min-width="140" />
          <el-table-column prop="licensePlateNumber" label="车牌号" min-width="140" />
          <el-table-column prop="driverName" label="司机姓名" width="120" />
          <el-table-column prop="driverPhone" label="司机手机号" width="140" />
          <el-table-column prop="validStart" label="有效开始" width="130">
            <template #default="{ row }">{{ row.validStart || "-" }}</template>
          </el-table-column>
          <el-table-column prop="validEnd" label="有效结束" width="130">
            <template #default="{ row }">{{ row.validEnd || "-" }}</template>
          </el-table-column>
          <el-table-column label="行驶证附件" width="110">
            <template #default="{ row }">
              <el-link v-if="row.licenseFileId" :href="fileUrl(row.licenseFileId)" target="_blank">查看</el-link>
              <span v-else class="sf-muted">—</span>
            </template>
          </el-table-column>
          <el-table-column prop="carType" label="车辆类型" width="140" />
          <el-table-column prop="dataSyncTime" label="同步时间" min-width="170">
            <template #default="{ row }">{{ row.dataSyncTime ? fmt(row.dataSyncTime) : "-" }}</template>
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
            :total="carsTotal"
            v-model:current-page="carsPage"
            v-model:page-size="carsPageSize"
            @change="loadCars"
          />
        </div>
      </el-tab-pane>

      <el-tab-pane label="出入记录" name="records">
        <div class="sf-filter" style="margin-bottom: 10px">
          <el-button type="primary" @click="openRecordCreate">新增记录</el-button>
          <el-button @click="loadRecords">刷新</el-button>
        </div>
        <el-table :data="records" v-loading="loadingRecords" style="width: 100%">
          <el-table-column prop="companyCode" label="企业编码" min-width="140" />
          <el-table-column prop="licensePlateNumber" label="车牌号" min-width="140" />
          <el-table-column prop="driverName" label="司机姓名" width="120">
            <template #default="{ row }">{{ row.driverName || "-" }}</template>
          </el-table-column>
          <el-table-column prop="carType" label="车辆类型" width="140" />
          <el-table-column prop="inOutState" label="出入状态" width="110">
            <template #default="{ row }">{{ row.inOutState === "IN" ? "进" : row.inOutState === "OUT" ? "出" : row.inOutState }}</template>
          </el-table-column>
          <el-table-column prop="inOutTime" label="进出时间" min-width="180">
            <template #default="{ row }">{{ fmt(row.inOutTime) }}</template>
          </el-table-column>
          <el-table-column label="车辆照片" width="110">
            <template #default="{ row }">
              <el-link v-if="row.imageFileId" :href="fileUrl(row.imageFileId)" target="_blank">查看</el-link>
              <span v-else class="sf-muted">—</span>
            </template>
          </el-table-column>
          <el-table-column label="操作" width="160" fixed="right">
            <template #default="{ row }">
              <el-button size="small" @click="openRecordEdit(row)">编辑</el-button>
              <el-button size="small" type="danger" plain @click="deleteRecord(row.id)">删除</el-button>
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
    </div>

    <el-dialog v-model="dialogVisible" :title="editing?.id ? '编辑车辆' : '新增车辆'" width="720px">
      <el-form label-width="110px">
        <div class="sf-form-grid">
          <el-form-item label="企业编码" required>
          <el-input v-model="form.companyCode" />
          </el-form-item>
          <el-form-item label="车牌号" required>
          <el-input v-model="form.licensePlateNumber" />
          </el-form-item>
          <el-form-item label="司机姓名">
          <el-input v-model="form.driverName" />
          </el-form-item>
          <el-form-item label="司机手机号">
          <el-input v-model="form.driverPhone" />
          </el-form-item>
          <el-form-item label="有效开始">
          <el-date-picker
            v-model="form.validStart"
            type="date"
            value-format="YYYY-MM-DD"
            placeholder="选择日期"
            style="width: 100%"
          />
          </el-form-item>
          <el-form-item label="有效结束">
          <el-date-picker
            v-model="form.validEnd"
            type="date"
            value-format="YYYY-MM-DD"
            placeholder="选择日期"
            style="width: 100%"
          />
          </el-form-item>
          <el-form-item label="行驶证附件" class="sf-form-full">
            <div style="display: flex; gap: 10px; align-items: center; width: 100%">
            <el-upload :show-file-list="false" :before-upload="uploadLicenseFile" accept=".jpg,.jpeg,image/jpeg">
              <el-button>上传</el-button>
            </el-upload>
            <el-link v-if="form.licenseFileId" :href="fileUrl(form.licenseFileId)" target="_blank">已上传</el-link>
            <span v-else class="sf-muted">未上传</span>
          </div>
          </el-form-item>
          <el-form-item label="车辆类型">
          <el-input v-model="form.carType" placeholder="危化/其他" />
          </el-form-item>
        </div>
      </el-form>
      <template #footer>
        <el-button @click="dialogVisible = false">取消</el-button>
        <el-button type="primary" :loading="saving" @click="onSave">保存</el-button>
      </template>
    </el-dialog>

    <el-dialog v-model="recordDialogVisible" :title="recordEditing?.id ? '编辑出入记录' : '新增出入记录'" width="680px">
      <el-form label-width="110px">
        <div class="sf-form-grid">
          <el-form-item label="企业编码">
            <el-input v-model="recordForm.companyCode" placeholder="可选" />
          </el-form-item>
          <el-form-item label="车牌号" required>
            <el-input v-model="recordForm.licensePlateNumber" />
          </el-form-item>
          <el-form-item label="车辆类型">
            <el-input v-model="recordForm.carType" />
          </el-form-item>
          <el-form-item label="进出状态" required>
            <el-select v-model="recordForm.inOutState" style="width: 100%">
              <el-option label="IN" value="IN" />
              <el-option label="OUT" value="OUT" />
            </el-select>
          </el-form-item>
          <el-form-item label="进出时间" required>
            <el-date-picker
              v-model="recordForm.inOutTime"
              type="datetime"
              value-format="x"
              placeholder="选择时间"
              style="width: 100%"
            />
          </el-form-item>
          <el-form-item label="图片" class="sf-form-full">
            <div style="display: flex; gap: 10px; align-items: center; width: 100%">
              <el-upload :show-file-list="false" :before-upload="uploadRecordImage" accept=".jpg,.jpeg,image/jpeg">
                <el-button>上传</el-button>
              </el-upload>
              <el-link v-if="recordForm.imageFileId" :href="fileUrl(recordForm.imageFileId)" target="_blank">已上传</el-link>
              <span v-else class="sf-muted">未上传</span>
            </div>
          </el-form-item>
        </div>
      </el-form>
      <template #footer>
        <el-button @click="recordDialogVisible = false">取消</el-button>
        <el-button type="primary" :loading="recordSaving" @click="saveRecord">保存</el-button>
      </template>
    </el-dialog>
  </div>
</template>

<script setup lang="ts">
import { reactive, ref, watch } from "vue";
import { ElMessage, ElMessageBox } from "element-plus";
import { fileUrl, http, rawHttp, uploadFile } from "@/api/http";

type PageResponse<T> = { list: T[]; page: number; pageSize: number; total: number };

type CarVO = {
  id: number;
  companyCode: string;
  licensePlateNumber: string;
  driverName: string | null;
  driverPhone: string | null;
  validStart: string | null;
  validEnd: string | null;
  licenseFileId: number | null;
  carType: string | null;
  dataSyncTime: number | null;
};
type CarInoutRecordVO = {
  id: number;
  companyCode: string | null;
  licensePlateNumber: string;
  carType: string | null;
  driverName: string | null;
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

const recordDialogVisible = ref(false);
const recordSaving = ref(false);
const recordEditing = ref<CarInoutRecordVO | null>(null);
const recordForm = reactive({
  companyCode: "",
  licensePlateNumber: "",
  carType: "",
  inOutState: "IN",
  inOutTime: "",
  imageFileId: null as number | null
});

const dialogVisible = ref(false);
const saving = ref(false);
const editing = ref<CarVO | null>(null);

const form = reactive({
  companyCode: "",
  licensePlateNumber: "",
  driverName: "",
  driverPhone: "",
  validStart: null as string | null,
  validEnd: null as string | null,
  licenseFileId: null as number | null,
  carType: ""
});

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

function openRecordCreate() {
  recordEditing.value = null;
  Object.assign(recordForm, {
    companyCode: "",
    licensePlateNumber: "",
    carType: "",
    inOutState: "IN",
    inOutTime: "",
    imageFileId: null
  });
  recordDialogVisible.value = true;
}

function openRecordEdit(row: CarInoutRecordVO) {
  recordEditing.value = row;
  Object.assign(recordForm, {
    companyCode: row.companyCode || "",
    licensePlateNumber: row.licensePlateNumber,
    carType: row.carType || "",
    inOutState: row.inOutState,
    inOutTime: String(row.inOutTime),
    imageFileId: row.imageFileId || null
  });
  recordDialogVisible.value = true;
}

async function uploadRecordImage(file: File) {
  if (!isJpg(file)) {
    ElMessage.warning("请上传 JPG 图片");
    return false;
  }
  try {
    const out = await uploadFile("car_inout", file);
    recordForm.imageFileId = out.id;
    ElMessage.success("上传成功");
  } catch (e: any) {
    ElMessage.error(e?.message || "上传失败");
  }
  return false;
}

async function saveRecord() {
  if (!recordForm.licensePlateNumber || !recordForm.inOutTime) {
    ElMessage.warning("请补全必填项");
    return;
  }
  recordSaving.value = true;
  try {
    const payload = {
      id: recordEditing.value?.id,
      companyCode: recordForm.companyCode || null,
      licensePlateNumber: recordForm.licensePlateNumber,
      carType: recordForm.carType || null,
      inOutState: recordForm.inOutState,
      inOutTime: Number(recordForm.inOutTime),
      imageFileId: recordForm.imageFileId
    };
    if (recordEditing.value?.id) {
      await http.put<void>("/api/v1/car-inout", payload);
    } else {
      await http.post<number>("/api/v1/car-inout", payload);
    }
    ElMessage.success("保存成功");
    recordDialogVisible.value = false;
    await loadRecords();
  } catch (e: any) {
    ElMessage.error(e?.message || "保存失败");
  } finally {
    recordSaving.value = false;
  }
}

async function deleteRecord(id: number) {
  const ok = await ElMessageBox.confirm("确认删除该记录？", "提示", { type: "warning" }).catch(() => null);
  if (!ok) return;
  try {
    await http.del<void>(`/api/v1/car-inout/${id}`);
    ElMessage.success("删除成功");
    await loadRecords();
  } catch (e: any) {
    ElMessage.error(e?.message || "删除失败");
  }
}

async function importCars(file: File) {
  const fd = new FormData();
  fd.append("file", file);
  try {
    const res = await rawHttp.post("/api/v1/cars/import", fd, {
      headers: { "Content-Type": "multipart/form-data" }
    });
    ElMessage.success(`导入完成：成功 ${res.successCount} 条，失败 ${res.failCount} 条`);
    await loadCars();
  } catch (e: any) {
    ElMessage.error(e?.message || "导入失败");
  }
  return false;
}

function openCreate() {
  editing.value = null;
  Object.assign(form, {
    companyCode: "",
    licensePlateNumber: "",
    driverName: "",
    driverPhone: "",
    validStart: null,
    validEnd: null,
    licenseFileId: null,
    carType: ""
  });
  dialogVisible.value = true;
}

function openEdit(row: CarVO) {
  editing.value = row;
  Object.assign(form, {
    companyCode: row.companyCode,
    licensePlateNumber: row.licensePlateNumber,
    driverName: row.driverName || "",
    driverPhone: row.driverPhone || "",
    validStart: row.validStart || null,
    validEnd: row.validEnd || null,
    licenseFileId: row.licenseFileId || null,
    carType: row.carType || ""
  });
  dialogVisible.value = true;
}

function isJpg(file: File) {
  const name = file.name.toLowerCase();
  return file.type === "image/jpeg" || name.endsWith(".jpg") || name.endsWith(".jpeg");
}

async function uploadLicenseFile(file: File) {
  if (!isJpg(file)) {
    ElMessage.warning("请上传 JPG 图片");
    return false;
  }
  try {
    const out = await uploadFile("car_license", file);
    form.licenseFileId = out.id;
    ElMessage.success("上传成功");
  } catch (e: any) {
    ElMessage.error(e?.message || "上传失败");
  }
  return false;
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
