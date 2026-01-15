<template>
  <div class="sf-page">
    <div class="sf-page-head">
      <div>
        <h2 class="sf-page-title">人员管理</h2>
        <div class="sf-page-sub">人员档案、证书与出入记录</div>
      </div>
      <div class="sf-page-actions">
        <el-input v-model="keyword" placeholder="姓名/身份证号码/企业编码" style="width: 240px" clearable />
        <el-button type="primary" @click="openCreate">新增人员</el-button>
        <el-upload :show-file-list="false" :before-upload="importPeople" accept=".csv">
          <el-button>导入</el-button>
        </el-upload>
        <el-button @click="loadPeople">刷新</el-button>
      </div>
    </div>

    <div class="sf-card sf-table-card">
      <el-tabs v-model="tab">
      <el-tab-pane label="人员档案" name="people">
        <el-table :data="people" v-loading="loadingPeople" style="width: 100%">
          <el-table-column prop="companyCode" label="企业编码" min-width="140" />
          <el-table-column prop="personName" label="姓名" width="120" />
          <el-table-column prop="idcardMasked" label="身份证号码" min-width="180" />
          <el-table-column prop="personType" label="人员类型" min-width="160" />
          <el-table-column prop="isCertified" label="持证" width="90">
            <template #default="{ row }">
              <span class="sf-chip" :style="row.isCertified === 1 ? 'border-color: rgba(51,209,122,0.25)' : ''">
                {{ row.isCertified === 1 ? "是" : "否" }}
              </span>
            </template>
          </el-table-column>
          <el-table-column label="头像照片" width="110">
            <template #default="{ row }">
              <el-link v-if="row.avatarFileId" :href="fileUrl(row.avatarFileId)" target="_blank">查看</el-link>
              <span v-else class="sf-muted">—</span>
            </template>
          </el-table-column>
          <el-table-column label="证书附件" width="110">
            <template #default="{ row }">
              <el-link v-if="row.certFileId" :href="fileUrl(row.certFileId)" target="_blank">查看</el-link>
              <span v-else class="sf-muted">—</span>
            </template>
          </el-table-column>
          <el-table-column prop="certExpireDate" label="证书到期日期" width="140">
            <template #default="{ row }">{{ row.certExpireDate || "-" }}</template>
          </el-table-column>
          <el-table-column prop="smsNotify" label="短信推送" width="110">
            <template #default="{ row }">
              <span class="sf-chip" :style="row.smsNotify === 1 ? 'border-color: rgba(63,120,255,0.35)' : ''">
                {{ row.smsNotify === 1 ? "是" : "否" }}
              </span>
            </template>
          </el-table-column>
          <el-table-column prop="phone" label="手机号" width="140" />
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
            :total="peopleTotal"
            v-model:current-page="peoplePage"
            v-model:page-size="peoplePageSize"
            @change="loadPeople"
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
          <el-table-column prop="personName" label="姓名" width="120" />
          <el-table-column prop="idcardMasked" label="身份证号码" min-width="180" />
          <el-table-column prop="personType" label="人员类型" min-width="140" />
          <el-table-column prop="inOutState" label="出入状态" width="110">
            <template #default="{ row }">{{ row.inOutState === "IN" ? "进" : row.inOutState === "OUT" ? "出" : row.inOutState }}</template>
          </el-table-column>
          <el-table-column prop="inOutTime" label="进出时间" min-width="180">
            <template #default="{ row }">{{ fmt(row.inOutTime) }}</template>
          </el-table-column>
          <el-table-column label="人员照片" width="110">
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

    <el-dialog v-model="dialogVisible" :title="editing?.id ? '编辑人员' : '新增人员'" width="720px">
      <el-form label-width="110px">
        <div class="sf-form-grid">
          <el-form-item label="企业编码" required>
          <el-input v-model="form.companyCode" />
          </el-form-item>
          <el-form-item label="姓名" required>
          <el-input v-model="form.personName" />
          </el-form-item>
          <el-form-item v-if="!editing?.id" label="身份证号码" required>
          <el-input v-model="form.idcard" />
          </el-form-item>
          <el-form-item label="人员类型" required>
          <el-input v-model="form.personType" placeholder="法定代表人/负责人/安全管理/特种作业/其他" />
          </el-form-item>
          <el-form-item label="持证">
          <el-switch v-model="form.isCertified" :active-value="1" :inactive-value="0" />
          </el-form-item>
          <el-form-item label="头像照片" class="sf-form-full">
            <div style="display: flex; gap: 10px; align-items: center; width: 100%">
            <el-upload :show-file-list="false" :before-upload="file => uploadPersonFile(file, 'avatar')" accept=".jpg,.jpeg,image/jpeg">
              <el-button>上传</el-button>
            </el-upload>
            <el-link v-if="form.avatarFileId" :href="fileUrl(form.avatarFileId)" target="_blank">已上传</el-link>
            <span v-else class="sf-muted">未上传</span>
          </div>
          </el-form-item>
          <el-form-item label="证书附件" class="sf-form-full">
            <div style="display: flex; gap: 10px; align-items: center; width: 100%">
            <el-upload :show-file-list="false" :before-upload="file => uploadPersonFile(file, 'cert')" accept=".jpg,.jpeg,image/jpeg">
              <el-button>上传</el-button>
            </el-upload>
            <el-link v-if="form.certFileId" :href="fileUrl(form.certFileId)" target="_blank">已上传</el-link>
            <span v-else class="sf-muted">未上传</span>
          </div>
          </el-form-item>
          <el-form-item label="证书到期">
          <el-date-picker
            v-model="form.certExpireDate"
            type="date"
            value-format="YYYY-MM-DD"
            placeholder="选择日期"
            style="width: 100%"
          />
          </el-form-item>
          <el-form-item label="短信推送">
          <el-switch v-model="form.smsNotify" :active-value="1" :inactive-value="0" />
          </el-form-item>
          <el-form-item label="电话">
          <el-input v-model="form.phone" />
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
          <el-form-item label="姓名" required>
            <el-input v-model="recordForm.personName" />
          </el-form-item>
          <el-form-item label="身份证号码" required>
            <el-input v-model="recordForm.idcard" />
          </el-form-item>
          <el-form-item label="类型" required>
            <el-input v-model="recordForm.personType" />
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

type PersonVO = {
  id: number;
  companyCode: string;
  personName: string;
  idcardMasked: string;
  personType: string;
  isCertified: number;
  phone: string | null;
  avatarFileId: number | null;
  certFileId: number | null;
  certExpireDate: string | null;
  smsNotify: number | null;
  dataSyncTime: number | null;
};

type PersonInoutRecordVO = {
  id: number;
  companyCode: string | null;
  idcard: string;
  idcardMasked: string;
  personName: string;
  personType: string;
  inOutState: string;
  inOutTime: number;
  imageFileId: number | null;
};

const tab = ref<"people" | "records">("people");
const keyword = ref("");

const loadingPeople = ref(false);
const people = ref<PersonVO[]>([]);
const peopleTotal = ref(0);
const peoplePage = ref(1);
const peoplePageSize = ref(20);

const loadingRecords = ref(false);
const records = ref<PersonInoutRecordVO[]>([]);
const recordsTotal = ref(0);
const recordsPage = ref(1);
const recordsPageSize = ref(20);

const recordDialogVisible = ref(false);
const recordSaving = ref(false);
const recordEditing = ref<PersonInoutRecordVO | null>(null);
const recordForm = reactive({
  companyCode: "",
  personName: "",
  idcard: "",
  personType: "",
  inOutState: "IN",
  inOutTime: "",
  imageFileId: null as number | null
});

const dialogVisible = ref(false);
const saving = ref(false);
const editing = ref<PersonVO | null>(null);

const form = reactive({
  companyCode: "",
  personName: "",
  idcard: "",
  personType: "",
  isCertified: 0,
  phone: "",
  avatarFileId: null as number | null,
  certFileId: null as number | null,
  certExpireDate: null as string | null,
  smsNotify: 0
});

function fmt(ts: number) {
  const d = new Date(ts);
  const pad = (n: number) => String(n).padStart(2, "0");
  return `${d.getFullYear()}-${pad(d.getMonth() + 1)}-${pad(d.getDate())} ${pad(d.getHours())}:${pad(d.getMinutes())}:${pad(d.getSeconds())}`;
}

async function loadPeople() {
  loadingPeople.value = true;
  try {
    const data = await http.get<PageResponse<PersonVO>>(
      `/api/v1/people?page=${peoplePage.value}&pageSize=${peoplePageSize.value}${keyword.value ? `&keyword=${encodeURIComponent(keyword.value)}` : ""}`
    );
    people.value = data.list;
    peopleTotal.value = data.total;
  } catch (e: any) {
    ElMessage.error(e?.message || "加载失败");
  } finally {
    loadingPeople.value = false;
  }
}

async function loadRecords() {
  loadingRecords.value = true;
  try {
    const data = await http.get<PageResponse<PersonInoutRecordVO>>(
      `/api/v1/person-inout?page=${recordsPage.value}&pageSize=${recordsPageSize.value}`
    );
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
    personName: "",
    idcard: "",
    personType: "",
    inOutState: "IN",
    inOutTime: "",
    imageFileId: null
  });
  recordDialogVisible.value = true;
}

function openRecordEdit(row: PersonInoutRecordVO) {
  recordEditing.value = row;
  Object.assign(recordForm, {
    companyCode: row.companyCode || "",
    personName: row.personName,
    idcard: row.idcard,
    personType: row.personType,
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
    const out = await uploadFile("person_inout", file);
    recordForm.imageFileId = out.id;
    ElMessage.success("上传成功");
  } catch (e: any) {
    ElMessage.error(e?.message || "上传失败");
  }
  return false;
}

async function saveRecord() {
  if (!recordForm.personName || !recordForm.idcard || !recordForm.personType || !recordForm.inOutTime) {
    ElMessage.warning("请补全必填项");
    return;
  }
  recordSaving.value = true;
  try {
    const payload = {
      id: recordEditing.value?.id,
      companyCode: recordForm.companyCode || null,
      personName: recordForm.personName,
      idcard: recordForm.idcard,
      personType: recordForm.personType,
      inOutState: recordForm.inOutState,
      inOutTime: Number(recordForm.inOutTime),
      imageFileId: recordForm.imageFileId
    };
    if (recordEditing.value?.id) {
      await http.put<void>("/api/v1/person-inout", payload);
    } else {
      await http.post<number>("/api/v1/person-inout", payload);
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
    await http.del<void>(`/api/v1/person-inout/${id}`);
    ElMessage.success("删除成功");
    await loadRecords();
  } catch (e: any) {
    ElMessage.error(e?.message || "删除失败");
  }
}

async function importPeople(file: File) {
  const fd = new FormData();
  fd.append("file", file);
  try {
    const res = await rawHttp.post("/api/v1/people/import", fd, {
      headers: { "Content-Type": "multipart/form-data" }
    });
    ElMessage.success(`导入完成：成功 ${res.successCount} 条，失败 ${res.failCount} 条`);
    await loadPeople();
  } catch (e: any) {
    ElMessage.error(e?.message || "导入失败");
  }
  return false;
}

function openCreate() {
  editing.value = null;
  Object.assign(form, {
    companyCode: "",
    personName: "",
    idcard: "",
    personType: "",
    isCertified: 0,
    phone: "",
    avatarFileId: null,
    certFileId: null,
    certExpireDate: null,
    smsNotify: 0
  });
  dialogVisible.value = true;
}

function openEdit(row: PersonVO) {
  editing.value = row;
  Object.assign(form, {
    companyCode: row.companyCode,
    personName: row.personName,
    idcard: "",
    personType: row.personType,
    isCertified: row.isCertified,
    phone: row.phone || "",
    avatarFileId: row.avatarFileId || null,
    certFileId: row.certFileId || null,
    certExpireDate: row.certExpireDate || null,
    smsNotify: row.smsNotify ?? 0
  });
  dialogVisible.value = true;
}

function isJpg(file: File) {
  const name = file.name.toLowerCase();
  return file.type === "image/jpeg" || name.endsWith(".jpg") || name.endsWith(".jpeg");
}

async function uploadPersonFile(file: File, bizType: "avatar" | "cert") {
  if (!isJpg(file)) {
    ElMessage.warning("请上传 JPG 图片");
    return false;
  }
  try {
    const out = await uploadFile(bizType, file);
    if (bizType === "avatar") {
      form.avatarFileId = out.id;
    } else {
      form.certFileId = out.id;
    }
    ElMessage.success("上传成功");
  } catch (e: any) {
    ElMessage.error(e?.message || "上传失败");
  }
  return false;
}

async function onSave() {
  if (!form.companyCode || !form.personName || (!editing.value?.id && !form.idcard) || !form.personType) {
    ElMessage.warning("请补全必填项");
    return;
  }
  saving.value = true;
  try {
    if (!editing.value?.id) {
      await http.post<number>("/api/v1/people", { ...form });
    } else {
      await http.put<void>("/api/v1/people", { id: editing.value.id, ...form });
    }
    ElMessage.success("保存成功");
    dialogVisible.value = false;
    await loadPeople();
  } catch (e: any) {
    ElMessage.error(e?.message || "保存失败");
  } finally {
    saving.value = false;
  }
}

async function onDelete(id: number) {
  const ok = await ElMessageBox.confirm("确认删除该人员？", "提示", { type: "warning" }).catch(() => null);
  if (!ok) return;
  try {
    await http.del<void>(`/api/v1/people/${id}`);
    ElMessage.success("删除成功");
    await loadPeople();
  } catch (e: any) {
    ElMessage.error(e?.message || "删除失败");
  }
}

watch([peoplePage, peoplePageSize], () => loadPeople());
watch([recordsPage, recordsPageSize], () => loadRecords());
watch(keyword, () => {
  peoplePage.value = 1;
  loadPeople();
});
watch(tab, () => {
  if (tab.value === "records") loadRecords();
});

loadPeople();
</script>
