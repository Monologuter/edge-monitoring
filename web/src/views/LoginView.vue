<template>
  <div class="wrap">
    <div class="panel sf-card sf-glow">
      <div class="head">
        <div class="mark">
          <div class="markInner"></div>
        </div>
        <div>
          <div class="title">安全监测预警系统</div>
          <div class="sub">企业级 · 风险感知 · 预警闭环</div>
        </div>
      </div>

      <el-form :model="form" @keyup.enter="onSubmit">
        <el-form-item>
          <el-input v-model="form.username" placeholder="用户名" size="large" />
        </el-form-item>
        <el-form-item>
          <el-input v-model="form.password" placeholder="密码" type="password" show-password size="large" />
        </el-form-item>
        <el-button type="primary" size="large" style="width: 100%" :loading="loading" @click="onSubmit">
          登录
        </el-button>
      </el-form>

      <div class="tip sf-muted">
        默认管理员：<span class="mono">admin</span> / <span class="mono">Admin@123456</span>
      </div>
    </div>

    <div class="bgArt" aria-hidden="true">
      <div class="orb o1"></div>
      <div class="orb o2"></div>
      <div class="orb o3"></div>
      <div class="grid"></div>
    </div>
  </div>
</template>

<script setup lang="ts">
import { reactive, ref } from "vue";
import { ElMessage } from "element-plus";
import { useAuthStore } from "@/store/auth";
import router from "@/router";

const auth = useAuthStore();
const loading = ref(false);

const form = reactive({
  username: "admin",
  password: "Admin@123456"
});

async function onSubmit() {
  if (!form.username || !form.password) {
    ElMessage.warning("请输入用户名和密码");
    return;
  }
  loading.value = true;
  try {
    await auth.login(form.username, form.password);
    await auth.fetchMe();
    router.replace("/dashboard");
  } catch (e: any) {
    ElMessage.error(e?.message || "登录失败");
  } finally {
    loading.value = false;
  }
}
</script>

<style scoped>
.wrap {
  height: 100vh;
  display: grid;
  place-items: center;
  position: relative;
  overflow: hidden;
  background: radial-gradient(circle at 20% 10%, rgba(47, 107, 255, 0.12), transparent 55%),
    radial-gradient(circle at 80% 0%, rgba(39, 183, 167, 0.12), transparent 50%),
    linear-gradient(180deg, #f3f6fb, #eaf0f7);
}

.panel {
  width: min(420px, calc(100vw - 40px));
  padding: 22px 20px 18px;
  z-index: 2;
  border: 1px solid rgba(15, 42, 74, 0.08);
  border-radius: 18px;
}

.head {
  display: flex;
  align-items: center;
  gap: 12px;
  margin-bottom: 14px;
}

.mark {
  width: 42px;
  height: 42px;
  border-radius: 14px;
  background: radial-gradient(circle at 25% 25%, rgba(39, 183, 167, 0.95), transparent 60%),
    radial-gradient(circle at 70% 15%, rgba(47, 107, 255, 0.85), transparent 60%),
    rgba(47, 107, 255, 0.08);
  border: 1px solid rgba(47, 107, 255, 0.2);
  position: relative;
}

.markInner {
  position: absolute;
  inset: 10px;
  border-radius: 12px;
  border: 1px dashed rgba(47, 107, 255, 0.35);
}

.title {
  font-family: var(--sf-font-display);
  font-weight: 800;
  letter-spacing: 0.4px;
}

.sub {
  margin-top: 2px;
  font-size: 12px;
  color: var(--sf-text-2);
}

.tip {
  margin-top: 12px;
  font-size: 12px;
}

.mono {
  font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, "Liberation Mono", "Courier New", monospace;
  color: rgba(42, 49, 64, 0.82);
}


.bgArt {
  position: absolute;
  inset: 0;
  pointer-events: none;
  z-index: 1;
}

.orb {
  position: absolute;
  border-radius: 999px;
  filter: blur(18px);
  opacity: 0.9;
}

.o1 {
  width: 420px;
  height: 420px;
  left: -120px;
  top: -120px;
  background: rgba(39, 183, 167, 0.2);
}

.o2 {
  width: 520px;
  height: 520px;
  right: -200px;
  top: -180px;
  background: rgba(47, 107, 255, 0.2);
}

.o3 {
  width: 560px;
  height: 560px;
  left: 40%;
  bottom: -280px;
  background: rgba(255, 183, 77, 0.18);
}

.grid {
  position: absolute;
  inset: 0;
  background-image: linear-gradient(rgba(51, 65, 85, 0.05) 1px, transparent 1px),
    linear-gradient(90deg, rgba(51, 65, 85, 0.05) 1px, transparent 1px);
  background-size: 42px 42px;
  mask-image: radial-gradient(circle at 50% 45%, rgba(0, 0, 0, 1) 0%, rgba(0, 0, 0, 0) 70%);
  opacity: 0.55;
}
</style>
