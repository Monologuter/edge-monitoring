<template>
  <div>
    <div style="display: flex; align-items: baseline; justify-content: space-between; margin-bottom: 14px">
      <h2 class="sf-title">人员管理</h2>
      <div style="display: flex; gap: 10px; align-items: center">
        <el-input v-model="keyword" placeholder="姓名/身份证/企业编码" style="width: 240px" clearable />
        <el-button type="primary" @click="openCreate">新增人员</el-button>
        <el-button @click="loadPeople">刷新</el-button>
      </div>
    </div>

    <el-tabs v-model="tab">
      <el-tab-pane label="人员档案" name="people">
        <el-table :data="people" v-loading="loadingPeople" style="width: 100%">
          <el-table-column prop="companyCode" label="企业编码" min-width="140" />
          <el-table-column prop="personName" label="姓名" width="120" />
          <el-table-column prop="idcardMasked" label="身份证" min-width="180" />
          <el-table-column prop="personType" label="类型" min-width="160" />
          <el-table-column prop="isCertified" label="持证" width="90">
            <template #default="{ row }">
              <span class="sf-chip" :style="row.isCertified === 1 ? 'border-color: rgba(51,209,122,0.25)' : ''">
                {{ row.isCertified === 1 ? "是" : "否" }}
              </span>
            </template>
          </el-table-column>
          <el-table-column prop="phone" label="电话" width="140" />
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
        <el-table :data="records" v-loading="loadingRecords" style="width: 100%">
          <el-table-column prop="personName" label="姓名" width="120" />
          <el-table-column prop="idcardMasked" label="身份证" min-width="180" />
          <el-table-column prop="personType" label="类型" min-width="140" />
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

    <el-dialog v-model="dialogVisible" :title="editing?.id ? '编辑人员' : '新增人员'" width="640px">
      <el-form label-width="90px">
        <el-form-item label="企业编码" required>
          <el-input v-model="form.companyCode" />
        </el-form-item>
        <el-form-item label="姓名" required>
          <el-input v-model="form.personName" />
        </el-form-item>
        <el-form-item v-if="!editing?.id" label="身份证" required>
          <el-input v-model="form.idcard" />
        </el-form-item>
        <el-form-item label="类型" required>
          <el-input v-model="form.personType" placeholder="法定代表人/负责人/安全管理/特种作业/其他" />
        </el-form-item>
        <el-form-item label="持证">
          <el-switch v-model="form.isCertified" :active-value="1" :inactive-value="0" />
        </el-form-item>
        <el-form-item label="电话">
          <el-input v-model="form.phone" />
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

type PersonVO = {
  id: number;
  companyCode: string;
  personName: string;
  idcardMasked: string;
  personType: string;
  isCertified: number;
  phone: string | null;
};

type PersonInoutRecordVO = {
  id: number;
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

const dialogVisible = ref(false);
const saving = ref(false);
const editing = ref<PersonVO | null>(null);

const form = reactive({
  companyCode: "",
  personName: "",
  idcard: "",
  personType: "",
  isCertified: 0,
  phone: ""
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

function openCreate() {
  editing.value = null;
  Object.assign(form, { companyCode: "", personName: "", idcard: "", personType: "", isCertified: 0, phone: "" });
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
    phone: row.phone || ""
  });
  dialogVisible.value = true;
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
