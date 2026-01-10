<template>
  <div>
    <div style="display: flex; align-items: baseline; justify-content: space-between; margin-bottom: 14px">
      <h2 class="sf-title">企业档案</h2>
      <div style="display: flex; gap: 10px; align-items: center">
        <el-input v-model="keyword" placeholder="企业编码/名称" style="width: 220px" clearable />
        <el-button type="primary" @click="openCreate">新增企业</el-button>
        <el-button @click="load">刷新</el-button>
      </div>
    </div>

    <el-table :data="list" v-loading="loading" style="width: 100%">
      <el-table-column prop="companyCode" label="企业编码" min-width="140" />
      <el-table-column prop="companyName" label="企业名称" min-width="220" />
      <el-table-column prop="companyStatus" label="状态" width="120" />
      <el-table-column prop="businessLicense" label="营业执照号" min-width="180" />
      <el-table-column label="营业执照附件" width="140">
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

    <el-dialog v-model="dialogVisible" :title="editing?.id ? '编辑企业' : '新增企业'" width="720px">
      <el-form label-width="110px">
        <el-form-item label="企业编码" required>
          <el-input v-model="form.companyCode" :disabled="Boolean(editing?.id)" />
        </el-form-item>
        <el-form-item label="企业名称" required>
          <el-input v-model="form.companyName" />
        </el-form-item>
        <el-form-item label="企业状态">
          <el-input v-model="form.companyStatus" placeholder="在业/停产…" />
        </el-form-item>
        <el-form-item label="营业执照号">
          <el-input v-model="form.businessLicense" />
        </el-form-item>
        <el-form-item label="执照附件">
          <div style="display: flex; gap: 10px; align-items: center; width: 100%">
            <el-upload :show-file-list="false" :before-upload="beforeUpload">
              <el-button>上传</el-button>
            </el-upload>
            <el-link v-if="form.businessLicenseFileId" :href="fileUrl(form.businessLicenseFileId)" target="_blank">已上传，点击查看</el-link>
            <span v-else class="sf-muted">未上传</span>
          </div>
        </el-form-item>
        <el-form-item label="经营范围">
          <el-input v-model="form.businessLicenseScope" type="textarea" :rows="2" />
        </el-form-item>
        <el-form-item label="发证机关">
          <el-input v-model="form.businessLicenseIssuingAuthority" />
        </el-form-item>
        <el-form-item label="地址">
          <el-input v-model="form.address" />
        </el-form-item>
        <el-form-item label="注册地址">
          <el-input v-model="form.registerAddress" />
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
import { http, uploadFile } from "@/api/http";
import { fileUrl } from "@/api/http";

type CompanyVO = {
  id: number;
  companyCode: string;
  companyName: string;
  businessLicense: string | null;
  businessLicenseFileId: number | null;
  businessLicenseScope: string | null;
  businessLicenseIssuingAuthority: string | null;
  address: string | null;
  registerAddress: string | null;
  companyStatus: string | null;
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
  companyStatus: "",
  businessLicense: "",
  businessLicenseFileId: null as number | null,
  businessLicenseScope: "",
  businessLicenseIssuingAuthority: "",
  address: "",
  registerAddress: ""
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
    companyStatus: "",
    businessLicense: "",
    businessLicenseFileId: null,
    businessLicenseScope: "",
    businessLicenseIssuingAuthority: "",
    address: "",
    registerAddress: ""
  });
  dialogVisible.value = true;
}

function openEdit(row: CompanyVO) {
  editing.value = row;
  Object.assign(form, {
    companyCode: row.companyCode,
    companyName: row.companyName,
    companyStatus: row.companyStatus || "",
    businessLicense: row.businessLicense || "",
    businessLicenseFileId: row.businessLicenseFileId || null,
    businessLicenseScope: row.businessLicenseScope || "",
    businessLicenseIssuingAuthority: row.businessLicenseIssuingAuthority || "",
    address: row.address || "",
    registerAddress: row.registerAddress || ""
  });
  dialogVisible.value = true;
}

async function beforeUpload(file: File) {
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

watch([page, pageSize], () => load());
watch(keyword, () => {
  page.value = 1;
  load();
});

load();
</script>
