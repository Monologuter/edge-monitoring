<template>
  <div class="sf-page page">
    <div class="sf-page-head">
      <div>
        <h2 class="sf-page-title">人员定位</h2>
        <div class="sf-page-sub">链接到现有人员定位系统</div>
      </div>
      <div class="sf-page-actions">
        <el-button type="primary" :disabled="!config?.personLocationUrl" @click="go">
          进入人员定位系统
        </el-button>
      </div>
    </div>

    <div class="sf-card card">
      <div class="label">系统地址</div>
      <div class="value">{{ config?.personLocationUrl || "未配置" }}</div>
      <div class="hint">如需调整地址，请在后端配置 app.external.person-location-url</div>
    </div>
  </div>
</template>

<script setup lang="ts">
import { onMounted, ref } from "vue";
import { ElMessage } from "element-plus";
import { http } from "@/api/http";

type SystemConfigVO = { personLocationUrl: string | null };

const config = ref<SystemConfigVO | null>(null);

async function load() {
  try {
    config.value = await http.get<SystemConfigVO>("/api/v1/system/config");
  } catch (e: any) {
    ElMessage.error(e?.message || "加载配置失败");
  }
}

function go() {
  if (!config.value?.personLocationUrl) return;
  window.open(config.value.personLocationUrl, "_blank");
}

onMounted(load);
</script>

<style scoped>
.page {
  display: grid;
  gap: 16px;
}

.card {
  padding: 18px;
}

.label {
  font-size: 12px;
  color: var(--sf-text-2);
}

.value {
  margin-top: 6px;
  font-size: 16px;
  font-weight: 600;
}

.hint {
  margin-top: 10px;
  color: var(--sf-text-2);
  font-size: 12px;
}
</style>
