<template>
  <div class="sf-page sf-device-api-keys">
    <div class="sf-header">
      <h1>设备API密钥管理</h1>
      <div class="sf-actions">
        <el-button type="primary" @click="openCreate">新增密钥</el-button>
      </div>
    </div>

    <el-table :data="list" stripe v-loading="loading">
      <el-table-column prop="apiKey" label="API Key" width="200" />
      <el-table-column prop="apiSecret" label="API Secret" width="150">
        <template #default="{ row }">
          <code class="sf-muted">{{ row.apiSecret }}</code>
        </template>
      </el-table-column>
      <el-table-column label="状态" width="80">
        <template #default="{ row }">
          <el-tag :type="row.enabled ? 'success' : 'danger'">
            {{ row.enabled ? "启用" : "禁用" }}
          </el-tag>
        </template>
      </el-table-column>
      <el-table-column prop="allowedIps" label="允许IP" width="200">
        <template #default="{ row }">
          <span class="sf-muted">{{ row.allowedIps || "不限" }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="rateLimitPerMinute" label="限流(次/分)" width="120" />
      <el-table-column label="操作" width="180" fixed="right">
        <template #default="{ row }">
          <el-button link type="primary" size="small" @click="openEdit(row)">编辑</el-button>
          <el-popconfirm title="确定删除此密钥?" @confirm="handleDelete(row.id)">
            <template #reference>
              <el-button link type="danger" size="small">删除</el-button>
            </template>
          </el-popconfirm>
        </template>
      </el-table-column>
    </el-table>

    <el-dialog v-model="dialogVisible" :title="editing ? '编辑密钥' : '新增密钥'" width="500px">
      <el-form :model="form" label-width="100px">
        <el-form-item label="API Key">
          <el-input v-model="form.apiKey" :disabled="editing" placeholder="请输入API Key" />
        </el-form-item>
        <el-form-item label="API Secret">
          <el-input v-model="form.apiSecret" type="password" show-password placeholder="请输入API Secret" />
        </el-form-item>
        <el-form-item label="状态">
          <el-switch v-model="form.enabled" :active-value="1" :inactive-value="0" />
        </el-form-item>
        <el-form-item label="允许IP">
          <el-input v-model="form.allowedIps" placeholder="例如: 192.168.1.0/24,10.0.0.1" />
        </el-form-item>
        <el-form-item label="限流(次/分)">
          <el-input-number v-model="form.rateLimitPerMinute" :min="1" :max="1000" />
        </el-form-item>
      </el-form>
      <template #footer>
        <el-button @click="dialogVisible = false">取消</el-button>
        <el-button type="primary" @click="submit">确定</el-button>
      </template>
    </el-dialog>
  </div>
</template>

<script setup lang="ts">
import { ref, onMounted } from "vue";
import { ElMessage } from "element-plus";
import { http } from "@/api/http";

interface DeviceApiKeyVO {
  id: number;
  apiKey: string;
  apiSecret: string;
  enabled: number;
  allowedIps: string;
  rateLimitPerMinute: number;
}

const list = ref<DeviceApiKeyVO[]>([]);
const loading = ref(false);
const dialogVisible = ref(false);
const editing = ref<DeviceApiKeyVO | null>(null);
const form = ref({
  apiKey: "",
  apiSecret: "",
  enabled: 1,
  allowedIps: "",
  rateLimitPerMinute: 60
});

async function load() {
  loading.value = true;
  try {
    const data = await http.get<DeviceApiKeyVO[]>("/api/v1/device-api-keys");
    list.value = data || [];
  } catch (e: any) {
    ElMessage.error(e?.message || "加载失败");
  } finally {
    loading.value = false;
  }
}

function openCreate() {
  editing.value = null;
  Object.assign(form.value, {
    apiKey: "",
    apiSecret: "",
    enabled: 1,
    allowedIps: "",
    rateLimitPerMinute: 60
  });
  dialogVisible.value = true;
}

function openEdit(row: DeviceApiKeyVO) {
  editing.value = row;
  Object.assign(form.value, {
    apiKey: row.apiKey,
    apiSecret: "", // 不回填密钥
    enabled: row.enabled,
    allowedIps: row.allowedIps || "",
    rateLimitPerMinute: row.rateLimitPerMinute
  });
  dialogVisible.value = true;
}

async function submit() {
  if (!form.value.apiKey) {
    ElMessage.warning("请输入API Key");
    return;
  }
  try {
    if (editing.value) {
      await http.put("/api/v1/device-api-keys", {
        id: editing.value.id,
        apiSecret: form.value.apiSecret || undefined,
        enabled: form.value.enabled,
        allowedIps: form.value.allowedIps || undefined,
        rateLimitPerMinute: form.value.rateLimitPerMinute
      });
      ElMessage.success("更新成功");
    } else {
      if (!form.value.apiSecret) {
        ElMessage.warning("请输入API Secret");
        return;
      }
      await http.post("/api/v1/device-api-keys", form.value);
      ElMessage.success("创建成功");
    }
    dialogVisible.value = false;
    await load();
  } catch (e: any) {
    ElMessage.error(e?.message || "操作失败");
  }
}

async function handleDelete(id: number) {
  try {
    await http.del(`/api/v1/device-api-keys/${id}`);
    ElMessage.success("删除成功");
    await load();
  } catch (e: any) {
    ElMessage.error(e?.message || "删除失败");
  }
}

onMounted(() => {
  load();
});
</script>

<style scoped>
.sf-device-api-keys {
  padding: 20px;
}
.sf-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 20px;
}
.sf-header h1 {
  margin: 0;
  font-size: 20px;
  font-weight: 500;
}
.sf-muted {
  color: #909399;
}
code.sf-muted {
  font-family: monospace;
  font-size: 12px;
}
</style>
