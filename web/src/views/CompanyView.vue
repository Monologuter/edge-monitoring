<template>
  <div class="sf-page">
    <div class="sf-page-head">
      <div>
        <h2 class="sf-page-title">企业档案</h2>
        <div class="sf-page-sub">企业基础信息与证照管理</div>
      </div>
      <div class="sf-page-actions">
        <el-input v-model="keyword" placeholder="企业编码/名称" style="width: 220px" clearable />
        <el-button type="primary" @click="openCreate">新增企业</el-button>
        <el-upload :show-file-list="false" :before-upload="importCompanies" accept=".csv">
          <el-button>导入</el-button>
        </el-upload>
        <el-button @click="load">刷新</el-button>
      </div>
    </div>

    <div class="sf-card sf-table-card">
      <el-table :data="list" v-loading="loading" style="width: 100%">
      <el-table-column type="expand" width="44">
        <template #default="{ row }">
          <div class="company-detail">
            <div class="detail-item"><span class="label">注册地址</span><span>{{ row.registerAddress || "—" }}</span></div>
            <div class="detail-item"><span class="label">仓储地址</span><span>{{ row.storageAddress || "—" }}</span></div>
            <div class="detail-item"><span class="label">企业地址</span><span>{{ row.address || "—" }}</span></div>
            <div class="detail-item"><span class="label">库房面积（m²）</span><span>{{ row.storeroomArea ?? "—" }}</span></div>
            <div class="detail-item"><span class="label">库区面积（m²）</span><span>{{ row.reservoirArea ?? "—" }}</span></div>
            <div class="detail-item"><span class="label">核定药量（kg）</span><span>{{ row.dosage ?? "—" }}</span></div>
            <div class="detail-item"><span class="label">经度</span><span>{{ row.longitude ?? "—" }}</span></div>
            <div class="detail-item"><span class="label">纬度</span><span>{{ row.latitude ?? "—" }}</span></div>
          </div>
        </template>
      </el-table-column>
      <el-table-column prop="companyCode" label="企业编码" min-width="140" />
      <el-table-column prop="companyName" label="企业名称" min-width="220" />
      <el-table-column prop="creditCode" label="信用代码" min-width="180" />
      <el-table-column prop="principalName" label="主要负责人" width="140" />
      <el-table-column prop="companyStatus" label="状态" width="120" />
      <el-table-column prop="businessLicense" label="许可证编号" min-width="180" />
      <el-table-column prop="businessLicenseStart" label="许可证开始" width="140">
        <template #default="{ row }">{{ row.businessLicenseStart || "-" }}</template>
      </el-table-column>
      <el-table-column prop="businessLicenseEnd" label="许可证截止" width="140">
        <template #default="{ row }">{{ row.businessLicenseEnd || "-" }}</template>
      </el-table-column>
      <el-table-column prop="businessLicenseIssuingAuthority" label="发证机关" min-width="160" />
      <el-table-column label="许可证附件" width="140">
        <template #default="{ row }">
          <el-link v-if="row.businessLicenseFileId" :href="fileUrl(row.businessLicenseFileId)" target="_blank">查看</el-link>
          <span v-else class="sf-muted">—</span>
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

    <el-dialog v-model="dialogVisible" :title="editing?.id ? '编辑企业' : '新增企业'" width="880px">
      <el-form label-width="120px">
        <div class="sf-form-grid">
          <el-form-item label="企业编码" required>
          <el-input v-model="form.companyCode" :disabled="Boolean(editing?.id)" />
          </el-form-item>
          <el-form-item label="企业名称" required>
          <el-input v-model="form.companyName" />
          </el-form-item>
          <el-form-item label="信用代码">
          <el-input v-model="form.creditCode" />
          </el-form-item>
          <el-form-item label="主要负责人">
          <el-input v-model="form.principalName" />
          </el-form-item>
          <el-form-item label="企业状态">
          <el-input v-model="form.companyStatus" placeholder="在业/停产…" />
          </el-form-item>
          <el-form-item label="许可证编号">
          <el-input v-model="form.businessLicense" />
          </el-form-item>
          <el-form-item label="许可证开始">
          <el-date-picker
            v-model="form.businessLicenseStart"
            type="date"
            value-format="YYYY-MM-DD"
            placeholder="选择日期"
            style="width: 100%"
          />
          </el-form-item>
          <el-form-item label="许可证截止">
          <el-date-picker
            v-model="form.businessLicenseEnd"
            type="date"
            value-format="YYYY-MM-DD"
            placeholder="选择日期"
            style="width: 100%"
          />
          </el-form-item>
          <el-form-item label="许可证附件" class="sf-form-full">
            <div style="display: flex; gap: 10px; align-items: center; width: 100%">
            <el-upload :show-file-list="false" :before-upload="beforeUpload" accept=".jpg,.jpeg,image/jpeg">
              <el-button>上传</el-button>
            </el-upload>
            <el-link v-if="form.businessLicenseFileId" :href="fileUrl(form.businessLicenseFileId)" target="_blank">已上传，点击查看</el-link>
            <span v-else class="sf-muted">未上传</span>
          </div>
          </el-form-item>
          <el-form-item label="经营范围" class="sf-form-full">
          <el-input v-model="form.businessLicenseScope" type="textarea" :rows="2" />
          </el-form-item>
          <el-form-item label="发证机关">
          <el-input v-model="form.businessLicenseIssuingAuthority" />
          </el-form-item>
          <el-form-item label="注册地址">
          <el-input v-model="form.registerAddress" />
          </el-form-item>
          <el-form-item label="仓储地址">
          <el-input v-model="form.storageAddress" />
          </el-form-item>
          <el-form-item label="企业地址">
          <el-input v-model="form.address" />
          </el-form-item>
          <el-form-item label="库房面积">
          <el-input-number v-model="form.storeroomArea" style="width: 100%" />
          </el-form-item>
          <el-form-item label="库区面积">
          <el-input-number v-model="form.reservoirArea" style="width: 100%" />
          </el-form-item>
          <el-form-item label="核定药量">
          <el-input-number v-model="form.dosage" style="width: 100%" />
          </el-form-item>
          <el-form-item label="经度">
          <el-input-number v-model="form.longitude" style="width: 100%" />
          </el-form-item>
          <el-form-item label="纬度">
          <el-input-number v-model="form.latitude" style="width: 100%" />
          </el-form-item>
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
import { fileUrl, http, rawHttp, uploadFile } from "@/api/http";

type CompanyVO = {
  id: number;
  companyCode: string;
  companyName: string;
  creditCode: string | null;
  principalName: string | null;
  businessLicense: string | null;
  businessLicenseFileId: number | null;
  businessLicenseScope: string | null;
  businessLicenseIssuingAuthority: string | null;
  businessLicenseStart: string | null;
  businessLicenseEnd: string | null;
  address: string | null;
  registerAddress: string | null;
  storageAddress: string | null;
  companyStatus: string | null;
  dosage: number | null;
  reservoirArea: number | null;
  storeroomArea: number | null;
  longitude: number | null;
  latitude: number | null;
};

type PageResponse<T> = { list: T[]; page: number; pageSize: number; total: number };

const keyword = ref("");
const page = ref(1);
const pageSize = ref(20);
const total = ref(0);
const loading = ref(false);
const list = ref<CompanyVO[]>([]);

const dialogVisible = ref(false);
const saving = ref(false);
const editing = ref<CompanyVO | null>(null);

const form = reactive({
  companyCode: "",
  companyName: "",
  creditCode: "",
  principalName: "",
  companyStatus: "",
  businessLicense: "",
  businessLicenseFileId: null as number | null,
  businessLicenseStart: "" as string | null,
  businessLicenseEnd: "" as string | null,
  businessLicenseScope: "",
  businessLicenseIssuingAuthority: "",
  address: "",
  registerAddress: "",
  storageAddress: "",
  dosage: null as number | null,
  reservoirArea: null as number | null,
  storeroomArea: null as number | null,
  longitude: null as number | null,
  latitude: null as number | null
});

async function load() {
  loading.value = true;
  try {
    const data = await http.get<PageResponse<CompanyVO>>(
      `/api/v1/companies?page=${page.value}&pageSize=${pageSize.value}${keyword.value ? `&keyword=${encodeURIComponent(keyword.value)}` : ""}`
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
  Object.assign(form, {
    companyCode: "",
    companyName: "",
    creditCode: "",
    principalName: "",
    companyStatus: "",
    businessLicense: "",
    businessLicenseFileId: null,
    businessLicenseStart: null,
    businessLicenseEnd: null,
    businessLicenseScope: "",
    businessLicenseIssuingAuthority: "",
    address: "",
    registerAddress: "",
    storageAddress: "",
    dosage: null,
    reservoirArea: null,
    storeroomArea: null,
    longitude: null,
    latitude: null
  });
  dialogVisible.value = true;
}

function openEdit(row: CompanyVO) {
  editing.value = row;
  Object.assign(form, {
    companyCode: row.companyCode,
    companyName: row.companyName,
    creditCode: row.creditCode || "",
    principalName: row.principalName || "",
    companyStatus: row.companyStatus || "",
    businessLicense: row.businessLicense || "",
    businessLicenseFileId: row.businessLicenseFileId || null,
    businessLicenseStart: row.businessLicenseStart || null,
    businessLicenseEnd: row.businessLicenseEnd || null,
    businessLicenseScope: row.businessLicenseScope || "",
    businessLicenseIssuingAuthority: row.businessLicenseIssuingAuthority || "",
    address: row.address || "",
    registerAddress: row.registerAddress || "",
    storageAddress: row.storageAddress || "",
    dosage: row.dosage ?? null,
    reservoirArea: row.reservoirArea ?? null,
    storeroomArea: row.storeroomArea ?? null,
    longitude: row.longitude ?? null,
    latitude: row.latitude ?? null
  });
  dialogVisible.value = true;
}

function isJpg(file: File) {
  const name = file.name.toLowerCase();
  return file.type === "image/jpeg" || name.endsWith(".jpg") || name.endsWith(".jpeg");
}

async function beforeUpload(file: File) {
  if (!isJpg(file)) {
    ElMessage.warning("请上传 JPG 图片");
    return false;
  }
  try {
    const out = await uploadFile("license", file);
    form.businessLicenseFileId = out.id;
    ElMessage.success("上传成功");
  } catch (e: any) {
    ElMessage.error(e?.message || "上传失败");
  }
  return false;
}

async function onSave() {
  if (!form.companyCode || !form.companyName) {
    ElMessage.warning("请填写企业编码与企业名称");
    return;
  }
  saving.value = true;
  try {
    if (!editing.value?.id) {
      await http.post<number>("/api/v1/companies", { ...form });
      ElMessage.success("新增成功");
    } else {
      await http.put<void>("/api/v1/companies", { id: editing.value.id, ...form });
      ElMessage.success("保存成功");
    }
    dialogVisible.value = false;
    await load();
  } catch (e: any) {
    ElMessage.error(e?.message || "保存失败");
  } finally {
    saving.value = false;
  }
}

async function onDelete(id: number) {
  const ok = await ElMessageBox.confirm("确认删除该企业？", "提示", { type: "warning" }).catch(() => null);
  if (!ok) return;
  try {
    await http.del<void>(`/api/v1/companies/${id}`);
    ElMessage.success("删除成功");
    await load();
  } catch (e: any) {
    ElMessage.error(e?.message || "删除失败");
  }
}

async function importCompanies(file: File) {
  const fd = new FormData();
  fd.append("file", file);
  try {
    const res = await rawHttp.post("/api/v1/companies/import", fd, {
      headers: { "Content-Type": "multipart/form-data" }
    });
    ElMessage.success(`导入完成：成功 ${res.successCount} 条，失败 ${res.failCount} 条`);
    await load();
  } catch (e: any) {
    ElMessage.error(e?.message || "导入失败");
  }
  return false;
}

watch([page, pageSize], () => load());
watch(keyword, () => {
  page.value = 1;
  load();
});

load();
</script>

<style scoped>
.company-detail {
  display: grid;
  grid-template-columns: repeat(4, minmax(0, 1fr));
  gap: 10px 16px;
  padding: 10px 12px;
  background: var(--sf-bg-2);
  border-radius: 12px;
  border: 1px solid var(--sf-outline);
}

.detail-item {
  display: flex;
  flex-direction: column;
  gap: 6px;
  font-size: 12px;
}

.detail-item .label {
  color: var(--sf-text-2);
}

@media (max-width: 1200px) {
  .company-detail {
    grid-template-columns: repeat(2, minmax(0, 1fr));
  }
}

@media (max-width: 720px) {
  .company-detail {
    grid-template-columns: 1fr;
  }
}
</style>
