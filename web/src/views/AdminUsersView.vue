<template>
  <div>
    <div style="display: flex; align-items: baseline; justify-content: space-between; margin-bottom: 14px">
      <h2 class="sf-title">用户管理</h2>
      <div style="display: flex; gap: 10px; align-items: center">
        <el-input v-model="keyword" placeholder="用户名/显示名" style="width: 220px" clearable />
        <el-button type="primary" @click="openCreate">新增用户</el-button>
        <el-button @click="load">刷新</el-button>
      </div>
    </div>

    <el-table :data="list" v-loading="loading" style="width: 100%">
      <el-table-column prop="id" label="ID" width="90" />
      <el-table-column prop="username" label="用户名" width="140" />
      <el-table-column prop="displayName" label="显示名" min-width="160" />
      <el-table-column prop="enabled" label="启用" width="90">
        <template #default="{ row }">
          <span class="sf-chip" :style="row.enabled === 1 ? 'border-color: rgba(51,209,122,0.25)' : ''">
            {{ row.enabled === 1 ? "启用" : "禁用" }}
          </span>
        </template>
      </el-table-column>
      <el-table-column label="角色" min-width="200">
        <template #default="{ row }">
          <span v-for="r in row.roleKeys" :key="r" class="sf-chip" style="margin-right: 6px">{{ r }}</span>
        </template>
      </el-table-column>
      <el-table-column label="数据范围(企业)" min-width="240">
        <template #default="{ row }">
          <span v-if="!row.companyCodes || row.companyCodes.length === 0" class="sf-muted">全量(ADMIN)或未配置</span>
          <span v-for="c in row.companyCodes" :key="c" class="sf-chip" style="margin-right: 6px">{{ c }}</span>
        </template>
      </el-table-column>
      <el-table-column label="操作" width="260" fixed="right">
        <template #default="{ row }">
          <el-button size="small" @click="openEdit(row)">编辑</el-button>
          <el-button size="small" type="warning" plain @click="resetPwd(row)">重置密码</el-button>
          <el-button size="small" type="danger" plain :disabled="row.username === 'admin'" @click="onDelete(row.id)">删除</el-button>
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

    <el-dialog v-model="dialogVisible" :title="editing?.id ? '编辑用户' : '新增用户'" width="720px">
      <el-form label-width="110px">
        <el-form-item label="用户名" required>
          <el-input v-model="form.username" :disabled="Boolean(editing?.id)" />
        </el-form-item>
        <el-form-item label="显示名" required>
          <el-input v-model="form.displayName" />
        </el-form-item>
        <el-form-item v-if="!editing?.id" label="初始密码" required>
          <el-input v-model="form.password" type="password" show-password placeholder="如 Operator@123456" />
        </el-form-item>
        <el-form-item label="启用">
          <el-switch v-model="form.enabled" :active-value="1" :inactive-value="0" />
        </el-form-item>
        <el-form-item label="角色">
          <el-select v-model="form.roleKeys" multiple filterable style="width: 100%" placeholder="选择角色">
            <el-option v-for="r in roleOptions" :key="r.roleKey" :label="`${r.roleName} (${r.roleKey})`" :value="r.roleKey" />
          </el-select>
        </el-form-item>
        <el-form-item label="数据范围(企业)">
          <el-select v-model="form.companyCodes" multiple filterable style="width: 100%" placeholder="选择企业（非 ADMIN 必填）">
            <el-option v-for="c in companyOptions" :key="c.companyCode" :label="`${c.companyName} (${c.companyCode})`" :value="c.companyCode" />
          </el-select>
          <div class="sf-muted" style="margin-top: 6px; font-size: 12px">
            ADMIN 角色默认全量；其他角色按这里选择的企业编码做数据隔离。
          </div>
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

type PageResponse<T> = { list: T[]; page: number; pageSize: number; total: number };

type AdminUserVO = {
  id: number;
  username: string;
  displayName: string;
  enabled: number;
  roleKeys: string[];
  companyCodes: string[];
};

type AdminRoleVO = { id: number; roleKey: string; roleName: string };
type CompanyVO = { id: number; companyCode: string; companyName: string };

const keyword = ref("");
const page = ref(1);
const pageSize = ref(20);
const total = ref(0);
const loading = ref(false);
const list = ref<AdminUserVO[]>([]);

const dialogVisible = ref(false);
const saving = ref(false);
const editing = ref<AdminUserVO | null>(null);

const roleOptions = ref<AdminRoleVO[]>([]);
const companyOptions = ref<CompanyVO[]>([]);

const form = reactive({
  username: "",
  displayName: "",
  password: "",
  enabled: 1,
  roleKeys: [] as string[],
  companyCodes: [] as string[]
});

async function loadMeta() {
  const roles = await http.get<PageResponse<any>>(`/api/v1/admin/roles?page=1&pageSize=200`);
  roleOptions.value = roles.list.map((r: any) => ({ id: r.id, roleKey: r.roleKey, roleName: r.roleName }));
  const companies = await http.get<PageResponse<any>>(`/api/v1/companies?page=1&pageSize=200`);
  companyOptions.value = companies.list.map((c: any) => ({ id: c.id, companyCode: c.companyCode, companyName: c.companyName }));
}

async function load() {
  loading.value = true;
  try {
    const data = await http.get<PageResponse<AdminUserVO>>(
      `/api/v1/admin/users?page=${page.value}&pageSize=${pageSize.value}${keyword.value ? `&keyword=${encodeURIComponent(keyword.value)}` : ""}`
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
  Object.assign(form, { username: "", displayName: "", password: "", enabled: 1, roleKeys: [], companyCodes: [] });
  dialogVisible.value = true;
}

function openEdit(row: AdminUserVO) {
  editing.value = row;
  Object.assign(form, {
    username: row.username,
    displayName: row.displayName,
    password: "",
    enabled: row.enabled,
    roleKeys: [...(row.roleKeys || [])],
    companyCodes: [...(row.companyCodes || [])]
  });
  dialogVisible.value = true;
}

async function onSave() {
  if (!form.username || !form.displayName) {
    ElMessage.warning("请填写用户名与显示名");
    return;
  }
  saving.value = true;
  try {
    if (!editing.value?.id) {
      if (!form.password) {
        ElMessage.warning("请填写初始密码");
        return;
      }
      await http.post<number>("/api/v1/admin/users", { ...form });
    } else {
      await http.put<void>("/api/v1/admin/users", { id: editing.value.id, displayName: form.displayName, enabled: form.enabled, roleKeys: form.roleKeys, companyCodes: form.companyCodes });
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

async function resetPwd(row: AdminUserVO) {
  const out = await ElMessageBox.prompt("请输入新密码（需包含大小写字母与数字/符号）", "重置密码", {
    confirmButtonText: "确定",
    cancelButtonText: "取消",
    inputType: "password",
    inputPlaceholder: "如 Operator@123456"
  }).catch(() => null);
  if (!out) return;
  try {
    await http.post<void>("/api/v1/admin/users/reset-password", { userId: row.id, newPassword: out.value });
    ElMessage.success("重置成功");
  } catch (e: any) {
    ElMessage.error(e?.message || "重置失败");
  }
}

async function onDelete(id: number) {
  const ok = await ElMessageBox.confirm("确认删除该用户？", "提示", { type: "warning" }).catch(() => null);
  if (!ok) return;
  try {
    await http.del<void>(`/api/v1/admin/users/${id}`);
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

loadMeta().then(load);
</script>

