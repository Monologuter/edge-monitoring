<template>
  <div>
    <div style="display: flex; align-items: baseline; justify-content: space-between; margin-bottom: 14px">
      <h2 class="sf-title">省厅对接</h2>
      <div style="display: flex; gap: 10px; align-items: center">
        <el-button type="primary" @click="openFeedback">提交反馈</el-button>
        <el-button @click="loadAll">刷新</el-button>
      </div>
    </div>

    <el-tabs v-model="tab">
      <el-tab-pane label="预警列表" name="warnings">
        <el-table :data="warnings" v-loading="loadingWarnings" style="width: 100%">
          <el-table-column prop="externalId" label="预警ID" min-width="160" />
          <el-table-column prop="riskLevel" label="风险等级" width="120" />
          <el-table-column prop="pushType" label="推送类型" width="120" />
          <el-table-column prop="warningTime" label="时间" min-width="180">
            <template #default="{ row }">{{ row.warningTime ? fmt(row.warningTime) : "—" }}</template>
          </el-table-column>
          <el-table-column prop="companyCode" label="企业编码" min-width="140" />
          <el-table-column label="原始报文" width="120">
            <template #default="{ row }">
              <el-button size="small" @click="showJson(row.rawJson)">查看</el-button>
            </template>
          </el-table-column>
        </el-table>
        <div style="display: flex; justify-content: flex-end; margin-top: 12px">
          <el-pagination
            background
            layout="prev, pager, next, sizes, total"
            :total="warningsTotal"
            v-model:current-page="warningsPage"
            v-model:page-size="warningsPageSize"
            @change="loadWarnings"
          />
        </div>
      </el-tab-pane>

      <el-tab-pane label="反馈记录" name="feedback">
        <el-table :data="feedback" v-loading="loadingFeedback" style="width: 100%">
          <el-table-column prop="externalId" label="预警ID" min-width="160" />
          <el-table-column prop="feedback" label="反馈" min-width="260" />
          <el-table-column prop="status" label="状态" width="120" />
          <el-table-column prop="lastError" label="错误" min-width="220" />
        </el-table>
        <div style="display: flex; justify-content: flex-end; margin-top: 12px">
          <el-pagination
            background
            layout="prev, pager, next, sizes, total"
            :total="feedbackTotal"
            v-model:current-page="feedbackPage"
            v-model:page-size="feedbackPageSize"
            @change="loadFeedback"
          />
        </div>
      </el-tab-pane>
    </el-tabs>

    <el-dialog v-model="feedbackDialog" title="提交省厅预警反馈" width="560px">
      <el-form label-width="90px">
        <el-form-item label="预警ID" required><el-input v-model="feedbackForm.externalId" /></el-form-item>
        <el-form-item label="反馈内容" required><el-input v-model="feedbackForm.feedback" type="textarea" :rows="3" /></el-form-item>
      </el-form>
      <template #footer>
        <el-button @click="feedbackDialog = false">取消</el-button>
        <el-button type="primary" :loading="saving" @click="submitFeedback">提交</el-button>
      </template>
    </el-dialog>

    <el-dialog v-model="jsonDialog" title="原始报文" width="760px">
      <el-input v-model="jsonText" type="textarea" :rows="16" />
      <template #footer><el-button @click="jsonDialog = false">关闭</el-button></template>
    </el-dialog>
  </div>
</template>

<script setup lang="ts">
import { reactive, ref, watch } from "vue";
import { ElMessage } from "element-plus";
import { http } from "@/api/http";

type PageResponse<T> = { list: T[]; page: number; pageSize: number; total: number };
type Warning = { id: number; externalId: string; companyCode: string | null; riskLevel: string | null; pushType: string | null; warningTime: number | null; rawJson: string };
type Feedback = { id: number; externalId: string; feedback: string; status: string; lastError: string | null };

const tab = ref<"warnings" | "feedback">("warnings");

const loadingWarnings = ref(false);
const warnings = ref<Warning[]>([]);
const warningsTotal = ref(0);
const warningsPage = ref(1);
const warningsPageSize = ref(20);

const loadingFeedback = ref(false);
const feedback = ref<Feedback[]>([]);
const feedbackTotal = ref(0);
const feedbackPage = ref(1);
const feedbackPageSize = ref(20);

const feedbackDialog = ref(false);
const saving = ref(false);
const feedbackForm = reactive({ externalId: "", feedback: "" });

const jsonDialog = ref(false);
const jsonText = ref("");

function fmt(ts: number) {
  const d = new Date(ts);
  const pad = (n: number) => String(n).padStart(2, "0");
  return `${d.getFullYear()}-${pad(d.getMonth() + 1)}-${pad(d.getDate())} ${pad(d.getHours())}:${pad(d.getMinutes())}:${pad(d.getSeconds())}`;
}

function showJson(text: string) {
  jsonText.value = text || "";
  jsonDialog.value = true;
}

function openFeedback() {
  Object.assign(feedbackForm, { externalId: "", feedback: "" });
  feedbackDialog.value = true;
}

async function loadWarnings() {
  loadingWarnings.value = true;
  try {
    const data = await http.get<PageResponse<Warning>>(`/api/v1/provincial/warnings?page=${warningsPage.value}&pageSize=${warningsPageSize.value}`);
    warnings.value = data.list;
    warningsTotal.value = data.total;
  } catch (e: any) {
    ElMessage.error(e?.message || "加载失败");
  } finally {
    loadingWarnings.value = false;
  }
}

async function loadFeedback() {
  loadingFeedback.value = true;
  try {
    const data = await http.get<PageResponse<Feedback>>(`/api/v1/provincial/feedback?page=${feedbackPage.value}&pageSize=${feedbackPageSize.value}`);
    feedback.value = data.list;
    feedbackTotal.value = data.total;
  } catch (e: any) {
    ElMessage.error(e?.message || "加载失败");
  } finally {
    loadingFeedback.value = false;
  }
}

async function submitFeedback() {
  if (!feedbackForm.externalId || !feedbackForm.feedback) {
    ElMessage.warning("请填写必填项");
    return;
  }
  saving.value = true;
  try {
    await http.post<void>("/api/v1/provincial/feedback", { ...feedbackForm });
    ElMessage.success("提交成功");
    feedbackDialog.value = false;
    tab.value = "feedback";
    await loadFeedback();
  } catch (e: any) {
    ElMessage.error(e?.message || "提交失败");
  } finally {
    saving.value = false;
  }
}

async function loadAll() {
  await Promise.all([loadWarnings(), loadFeedback()]);
}

watch([warningsPage, warningsPageSize], () => loadWarnings());
watch([feedbackPage, feedbackPageSize], () => loadFeedback());
watch(tab, () => {
  if (tab.value === "warnings") loadWarnings();
  else loadFeedback();
});

loadWarnings();
</script>

