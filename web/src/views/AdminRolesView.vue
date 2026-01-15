<template>
  <div class="sf-page">
    <div class="sf-page-head">
      <div>
        <h2 class="sf-page-title">角色管理</h2>
        <div class="sf-page-sub">角色权限与菜单授权</div>
      </div>
      <div class="sf-page-actions">
        <el-input v-model="keyword" placeholder="roleKey/roleName" style="width: 220px" clearable />
        <el-button type="primary" @click="openCreate">新增角色</el-button>
        <el-button @click="load">刷新</el-button>
      </div>
    </div>

    <div class="sf-card sf-table-card">
      <el-table :data="list" v-loading="loading" style="width: 100%">
      <el-table-column prop="id" label="ID" width="90" />
      <el-table-column prop="roleKey" label="roleKey" width="160" />
      <el-table-column prop="roleName" label="roleName" min-width="180" />
      <el-table-column label="权限数" width="110">
        <template #default="{ row }">{{ row.permissionKeys?.length || 0 }}</template>
      </el-table-column>
      <el-table-column label="菜单数" width="110">
        <template #default="{ row }">{{ row.menuKeys?.length || 0 }}</template>
      </el-table-column>
      <el-table-column label="操作" width="220" fixed="right">
        <template #default="{ row }">
          <el-button size="small" :disabled="row.roleKey === 'ADMIN'" @click="openEdit(row)">编辑</el-button>
          <el-button size="small" type="danger" plain :disabled="row.roleKey === 'ADMIN'" @click="onDelete(row.id)">删除</el-button>
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

    <el-dialog v-model="dialogVisible" :title="editing?.id ? '编辑角色' : '新增角色'" width="860px">
      <el-form label-width="90px">
        <div class="sf-form-grid">
          <el-form-item label="roleKey" required>
          <el-input v-model="form.roleKey" :disabled="Boolean(editing?.id)" placeholder="如 OPERATOR" />
          </el-form-item>
          <el-form-item label="roleName" required>
          <el-input v-model="form.roleName" placeholder="如 值班员" />
          </el-form-item>
          <el-form-item label="权限" required class="sf-form-full">
          <el-select v-model="form.permissionKeys" multiple filterable style="width: 100%" placeholder="选择权限">
            <el-option v-for="p in permissionOptions" :key="p" :label="p" :value="p" />
          </el-select>
          </el-form-item>
          <el-form-item label="菜单" required class="sf-form-full">
          <el-select v-model="form.menuKeys" multiple filterable style="width: 100%" placeholder="选择菜单">
            <el-option v-for="m in menuOptions" :key="m.menuKey" :label="`${m.menuName} (${m.menuKey})`" :value="m.menuKey" />
          </el-select>
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
import { http } from "@/api/http";

type PageResponse<T> = { list: T[]; page: number; pageSize: number; total: number };

type AdminRoleVO = {
  id: number;
  roleKey: string;
  roleName: string;
  permissionKeys: string[];
  menuKeys: string[];
};

type MenuVO = { id: number; menuKey: string; menuName: string; path: string; icon: string | null; sortNo: number };

const keyword = ref("");
const page = ref(1);
const pageSize = ref(20);
const total = ref(0);
const loading = ref(false);
const list = ref<AdminRoleVO[]>([]);

const permissionOptions = ref<string[]>([]);
const menuOptions = ref<MenuVO[]>([]);

const dialogVisible = ref(false);
const saving = ref(false);
const editing = ref<AdminRoleVO | null>(null);

const form = reactive({
  roleKey: "",
  roleName: "",
  permissionKeys: [] as string[],
  menuKeys: [] as string[]
});

async function loadMeta() {
  permissionOptions.value = await http.get<string[]>("/api/v1/admin/roles/meta/permissions");
  menuOptions.value = await http.get<MenuVO[]>("/api/v1/admin/roles/meta/menus");
}

async function load() {
  loading.value = true;
  try {
    const data = await http.get<PageResponse<AdminRoleVO>>(
      `/api/v1/admin/roles?page=${page.value}&pageSize=${pageSize.value}${keyword.value ? `&keyword=${encodeURIComponent(keyword.value)}` : ""}`
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
  Object.assign(form, { roleKey: "", roleName: "", permissionKeys: [], menuKeys: [] });
  dialogVisible.value = true;
}

function openEdit(row: AdminRoleVO) {
  editing.value = row;
  Object.assign(form, { roleKey: row.roleKey, roleName: row.roleName, permissionKeys: [...(row.permissionKeys || [])], menuKeys: [...(row.menuKeys || [])] });
  dialogVisible.value = true;
}

async function onSave() {
  if (!form.roleKey || !form.roleName) {
    ElMessage.warning("请填写 roleKey/roleName");
    return;
  }
  saving.value = true;
  try {
    await http.post<number>("/api/v1/admin/roles", { id: editing.value?.id ?? null, ...form });
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
  const ok = await ElMessageBox.confirm("确认删除该角色？", "提示", { type: "warning" }).catch(() => null);
  if (!ok) return;
  try {
    await http.del<void>(`/api/v1/admin/roles/${id}`);
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
