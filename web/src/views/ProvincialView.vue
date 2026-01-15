<template>
  <div class="sf-page">
    <div class="sf-page-head">
      <div>
        <h2 class="sf-page-title">省厅对接</h2>
        <div class="sf-page-sub">企业信息上报、预警上报、反馈拉取与报文查看</div>
      </div>
      <div class="sf-page-actions">
        <el-button type="primary" @click="handleReportUnit">上报企业信息</el-button>
        <el-button type="warning" @click="handleReportWarning">上报预警信息</el-button>
        <el-button @click="loadAll">刷新</el-button>
      </div>
    </div>

    <div class="sf-card sf-table-card">
      <el-tabs v-model="tab">
        <!-- 企业信息上报标签页 -->
        <el-tab-pane label="企业信息上报" name="unitReport">
          <div class="sf-form-section">
            <h3 class="sf-form-section-title">企业基础信息</h3>
            <el-form :model="unitForm" label-width="140px" :rules="unitRules" ref="unitFormRef">
              <div class="sf-form-grid">
                <el-form-item label="企业编码" prop="unitCode">
                  <el-input v-model="unitForm.unitCode" placeholder="请输入企业编码" />
                </el-form-item>
                <el-form-item label="企业名称" prop="unitName">
                  <el-input v-model="unitForm.unitName" placeholder="请输入企业名称" />
                </el-form-item>
                <el-form-item label="企业类型" prop="unitType">
                  <el-select v-model="unitForm.unitType" placeholder="请选择企业类型" style="width: 100%">
                    <el-option label="危化品生产企业" value="危化品生产企业" />
                    <el-option label="危化品经营企业" value="危化品经营企业" />
                    <el-option label="危化品使用企业" value="危化品使用企业" />
                    <el-option label="仓储企业" value="仓储企业" />
                    <el-option label="运输企业" value="运输企业" />
                  </el-select>
                </el-form-item>
                <el-form-item label="统一社会信用代码" prop="creditCode">
                  <el-input v-model="unitForm.creditCode" placeholder="请输入信用代码" />
                </el-form-item>
                <el-form-item label="法定代表人" prop="legalPerson">
                  <el-input v-model="unitForm.legalPerson" placeholder="请输入法定代表人" />
                </el-form-item>
                <el-form-item label="联系人" prop="contactPerson">
                  <el-input v-model="unitForm.contactPerson" placeholder="请输入联系人" />
                </el-form-item>
                <el-form-item label="联系电话" prop="contactPhone">
                  <el-input v-model="unitForm.contactPhone" placeholder="请输入联系电话" />
                </el-form-item>
                <el-form-item label="行政区划代码" prop="areaCode">
                  <el-input v-model="unitForm.areaCode" placeholder="请输入行政区划代码" />
                </el-form-item>
                <el-form-item label="企业地址" prop="address" class="sf-form-full">
                  <el-input v-model="unitForm.address" placeholder="请输入企业地址" />
                </el-form-item>
                <el-form-item label="经度" prop="longitude">
                  <el-input-number v-model="unitForm.longitude" :precision="6" :step="0.000001" style="width: 100%" />
                </el-form-item>
                <el-form-item label="纬度" prop="latitude">
                  <el-input-number v-model="unitForm.latitude" :precision="6" :step="0.000001" style="width: 100%" />
                </el-form-item>
                <el-form-item label="成立日期" prop="establishDate">
                  <el-date-picker v-model="unitForm.establishDate" type="date" placeholder="选择日期" value-format="YYYY-MM-DD" style="width: 100%" />
                </el-form-item>
                <el-form-item label="注册资本" prop="registerCapital">
                  <el-input v-model="unitForm.registerCapital" placeholder="请输入注册资本" />
                </el-form-item>
                <el-form-item label="企业状态" prop="unitStatus">
                  <el-select v-model="unitForm.unitStatus" placeholder="请选择企业状态" style="width: 100%">
                    <el-option label="在营" value="在营" />
                    <el-option label="停业" value="停业" />
                    <el-option label="注销" value="注销" />
                  </el-select>
                </el-form-item>
                <el-form-item label="营业执照" prop="businessLicense" class="sf-form-full">
                  <el-input v-model="unitForm.businessLicense" placeholder="请输入营业执照编号" />
                </el-form-item>
                <el-form-item label="最大储量" prop="maxCapacity">
                  <el-input v-model="unitForm.maxCapacity" placeholder="请输入最大储量" />
                </el-form-item>
                <el-form-item label="应急预案" prop="emergencyPlan">
                  <el-input v-model="unitForm.emergencyPlan" placeholder="请输入预案编号" />
                </el-form-item>
                <el-form-item label="应急联系人" prop="emergencyContact">
                  <el-input v-model="unitForm.emergencyContact" placeholder="请输入应急联系人" />
                </el-form-item>
                <el-form-item label="应急联系电话" prop="emergencyPhone">
                  <el-input v-model="unitForm.emergencyPhone" placeholder="请输入应急联系电话" />
                </el-form-item>
                <el-form-item label="危险化学品类别" class="sf-form-full">
                  <el-checkbox-group v-model="unitForm.hazardCategories">
                    <el-checkbox label="爆炸品" />
                    <el-checkbox label="压缩气体" />
                    <el-checkbox label="易燃液体" />
                    <el-checkbox label="易燃固体" />
                    <el-checkbox label="氧化剂" />
                    <el-checkbox label="有毒品" />
                    <el-checkbox label="放射性物品" />
                    <el-checkbox label="腐蚀品" />
                  </el-checkbox-group>
                </el-form-item>
              </div>
              <el-form-item>
                <el-button type="primary" @click="submitUnitReport" :loading="unitSubmitting">提交上报</el-button>
                <el-button @click="resetUnitForm">重置</el-button>
              </el-form-item>
            </el-form>
          </div>
        </el-tab-pane>

        <!-- 预警信息上报标签页 -->
        <el-tab-pane label="预警信息上报" name="warningReport">
          <div class="sf-form-section">
            <h3 class="sf-form-section-title">预警信息上报</h3>
            <el-form :model="warningForm" label-width="140px" :rules="warningRules" ref="warningFormRef">
              <div class="sf-form-grid">
                <el-form-item label="预警ID" prop="warningId">
                  <el-input v-model="warningForm.warningId" placeholder="系统自动生成或手动输入" />
                </el-form-item>
                <el-form-item label="企业编码" prop="unitCode">
                  <el-input v-model="warningForm.unitCode" placeholder="请输入企业编码" />
                </el-form-item>
                <el-form-item label="企业名称" prop="unitName">
                  <el-input v-model="warningForm.unitName" placeholder="请输入企业名称" />
                </el-form-item>
                <el-form-item label="预警类型" prop="warningType">
                  <el-select v-model="warningForm.warningType" placeholder="请选择预警类型" style="width: 100%">
                    <el-option label="温度超标" value="温度超标" />
                    <el-option label="湿度超标" value="湿度超标" />
                    <el-option label="液位超标" value="液位超标" />
                    <el-option label="非法入侵" value="非法入侵" />
                    <el-option label="设备故障" value="设备故障" />
                    <el-option label="其他" value="其他" />
                  </el-select>
                </el-form-item>
                <el-form-item label="预警等级" prop="warningLevel">
                  <el-select v-model="warningForm.warningLevel" placeholder="请选择预警等级" style="width: 100%">
                    <el-option label="高危" value="高危" />
                    <el-option label="中危" value="中危" />
                    <el-option label="低危" value="低危" />
                  </el-select>
                </el-form-item>
                <el-form-item label="预警时间" prop="warningTime">
                  <el-date-picker v-model="warningForm.warningTime" type="datetime" placeholder="选择时间" value-format="x" style="width: 100%" />
                </el-form-item>
                <el-form-item label="预警状态" prop="warningStatus">
                  <el-select v-model="warningForm.warningStatus" placeholder="请选择预警状态" style="width: 100%">
                    <el-option label="未处理" value="未处理" />
                    <el-option label="处理中" value="处理中" />
                    <el-option label="已处理" value="已处理" />
                  </el-select>
                </el-form-item>
                <el-form-item label="预警内容" prop="warningContent" class="sf-form-full">
                  <el-input v-model="warningForm.warningContent" type="textarea" :rows="3" placeholder="请输入预警内容描述" />
                </el-form-item>
                <el-form-item label="设备编码" prop="deviceCode">
                  <el-input v-model="warningForm.deviceCode" placeholder="请输入设备编码" />
                </el-form-item>
                <el-form-item label="设备名称" prop="deviceName">
                  <el-input v-model="warningForm.deviceName" placeholder="请输入设备名称" />
                </el-form-item>
                <el-form-item label="危化品类别" prop="hazardCategory">
                  <el-input v-model="warningForm.hazardCategory" placeholder="请输入危化品类别" />
                </el-form-item>
                <el-form-item label="储量/容量" prop="capacity">
                  <el-input v-model="warningForm.capacity" placeholder="请输入储量或容量" />
                </el-form-item>
                <el-form-item label="实际数量" prop="actualAmount">
                  <el-input v-model="warningForm.actualAmount" placeholder="请输入实际数量" />
                </el-form-item>
                <el-form-item label="停业类型" prop="stopType">
                  <el-select v-model="warningForm.stopType" placeholder="请选择停业类型" style="width: 100%">
                    <el-option label="停业" value="停业" />
                    <el-option label="停产" value="停产" />
                    <el-option label="停止使用" value="停止使用" />
                  </el-select>
                </el-form-item>
                <el-form-item label="停业原因" prop="stopReason">
                  <el-input v-model="warningForm.stopReason" placeholder="请输入停业原因" />
                </el-form-item>
                <el-form-item label="经度" prop="longitude">
                  <el-input-number v-model="warningForm.longitude" :precision="6" :step="0.000001" style="width: 100%" />
                </el-form-item>
                <el-form-item label="纬度" prop="latitude">
                  <el-input-number v-model="warningForm.latitude" :precision="6" :step="0.000001" style="width: 100%" />
                </el-form-item>
                <el-form-item label="处理结果" class="sf-form-full">
                  <el-input v-model="warningForm.handleResult" type="textarea" :rows="2" placeholder="请输入处理结果" />
                </el-form-item>
                <el-form-item label="处理人">
                  <el-input v-model="warningForm.handlePerson" placeholder="请输入处理人" />
                </el-form-item>
                <el-form-item label="反馈内容" class="sf-form-full">
                  <el-input v-model="warningForm.feedback" type="textarea" :rows="2" placeholder="请输入反馈内容" />
                </el-form-item>
              </div>
              <el-form-item>
                <el-button type="primary" @click="submitWarningReport" :loading="warningSubmitting">提交上报</el-button>
                <el-button @click="resetWarningForm">重置</el-button>
              </el-form-item>
            </el-form>
          </div>
        </el-tab-pane>

        <!-- 预警列表标签页（从省厅拉取） -->
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

        <!-- 反馈记录标签页 -->
        <el-tab-pane label="反馈记录" name="feedback">
          <el-table :data="feedback" v-loading="loadingFeedback" style="width: 100%">
            <el-table-column prop="externalId" label="预警ID" min-width="160" />
            <el-table-column prop="feedback" label="反馈" min-width="260" />
            <el-table-column prop="status" label="状态" width="120">
              <template #default="{ row }">
                <el-tag :type="row.status === 'SUCCESS' ? 'success' : 'warning'">
                  {{ row.status === 'SUCCESS' ? '成功' : row.status === 'PENDING' ? '待发送' : '失败' }}
                </el-tag>
              </template>
            </el-table-column>
            <el-table-column prop="lastError" label="错误信息" min-width="220" />
            <el-table-column label="操作" width="120">
              <template #default="{ row }">
                <el-button size="small" v-if="row.status === 'FAILED'" @click="retryFeedback(row.externalId)">重试</el-button>
              </template>
            </el-table-column>
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

        <!-- 上报记录标签页 -->
        <el-tab-pane label="上报记录" name="reportLogs">
          <div class="sf-toolbar">
            <el-select v-model="logTypeFilter" placeholder="筛选类型" clearable style="width: 150px">
              <el-option label="企业信息上报" value="UNIT" />
              <el-option label="预警信息上报" value="WARNING" />
              <el-option label="反馈拉取" value="FEEDBACK" />
            </el-select>
            <el-select v-model="logStatusFilter" placeholder="筛选状态" clearable style="width: 150px">
              <el-option label="成功" value="SUCCESS" />
              <el-option label="失败" value="FAILED" />
              <el-option label="处理中" value="PENDING" />
            </el-select>
            <el-button @click="loadReportLogs">查询</el-button>
          </div>
          <el-table :data="reportLogs" v-loading="loadingLogs" style="width: 100%">
            <el-table-column prop="reportType" label="上报类型" width="140">
              <template #default="{ row }">
                {{ row.reportType === 'UNIT' ? '企业信息' : row.reportType === 'WARNING' ? '预警信息' : '反馈' }}
              </template>
            </el-table-column>
            <el-table-column prop="businessId" label="业务ID" min-width="180" />
            <el-table-column prop="status" label="状态" width="100">
              <template #default="{ row }">
                <el-tag :type="row.status === 'SUCCESS' ? 'success' : row.status === 'FAILED' ? 'danger' : 'warning'">
                  {{ row.status === 'SUCCESS' ? '成功' : row.status === 'FAILED' ? '失败' : '处理中' }}
                </el-tag>
              </template>
            </el-table-column>
            <el-table-column prop="retryCount" label="重试次数" width="100" />
            <el-table-column prop="errorMessage" label="错误信息" min-width="200" />
            <el-table-column prop="reportTime" label="上报时间" width="180">
              <template #default="{ row }">{{ row.reportTime ? fmt(row.reportTime) : '—' }}</template>
            </el-table-column>
            <el-table-column label="操作" width="120">
              <template #default="{ row }">
                <el-button size="small" v-if="row.status === 'FAILED'" @click="retryReport(row.id)">重试</el-button>
                <el-button size="small" @click="showReportDetail(row)">详情</el-button>
              </template>
            </el-table-column>
          </el-table>
          <div style="display: flex; justify-content: flex-end; margin-top: 12px">
            <el-pagination
              background
              layout="prev, pager, next, sizes, total"
              :total="logsTotal"
              v-model:current-page="logsPage"
              v-model:page-size="logsPageSize"
              @change="loadReportLogs"
            />
          </div>
        </el-tab-pane>

        <!-- 预警反馈拉取标签页 -->
        <el-tab-pane label="拉取省厅反馈" name="fetchFeedback">
          <div class="sf-form-section">
            <h3 class="sf-form-section-title">从省厅拉取预警反馈</h3>
            <el-form label-width="140px">
              <el-form-item label="预警ID">
                <el-input v-model="fetchWarningId" placeholder="请输入要查询的预警ID" style="width: 400px" />
              </el-form-item>
              <el-form-item>
                <el-button type="primary" @click="fetchFeedbackFromProvincial" :loading="fetching">拉取反馈</el-button>
              </el-form-item>
            </el-form>
          </div>
          <div v-if="fetchedFeedback.length > 0">
            <h4 class="sf-form-section-title">反馈结果</h4>
            <el-table :data="fetchedFeedback" style="width: 100%">
              <el-table-column prop="warningId" label="预警ID" min-width="160" />
              <el-table-column prop="feedback" label="反馈内容" min-width="300" />
              <el-table-column prop="feedbackPerson" label="反馈人" width="120" />
              <el-table-column prop="feedbackTime" label="反馈时间" min-width="180" />
              <el-table-column prop="handleResult" label="处理结果" min-width="200" />
            </el-table>
          </div>
        </el-tab-pane>
      </el-tabs>
    </div>

    <!-- 原始报文对话框 -->
    <el-dialog v-model="jsonDialog" title="原始报文" width="760px">
      <el-input v-model="jsonText" type="textarea" :rows="16" readonly />
      <template #footer><el-button @click="jsonDialog = false">关闭</el-button></template>
    </el-dialog>

    <!-- 上报详情对话框 -->
    <el-dialog v-model="detailDialog" title="上报详情" width="760px">
      <div v-if="currentDetail">
        <el-descriptions :column="2" border>
          <el-descriptions-item label="上报类型">{{ currentDetail.reportType === 'UNIT' ? '企业信息' : currentDetail.reportType === 'WARNING' ? '预警信息' : '反馈' }}</el-descriptions-item>
          <el-descriptions-item label="状态">
            <el-tag :type="currentDetail.status === 'SUCCESS' ? 'success' : currentDetail.status === 'FAILED' ? 'danger' : 'warning'">
              {{ currentDetail.status === 'SUCCESS' ? '成功' : currentDetail.status === 'FAILED' ? '失败' : '处理中' }}
            </el-tag>
          </el-descriptions-item>
          <el-descriptions-item label="业务ID">{{ currentDetail.businessId }}</el-descriptions-item>
          <el-descriptions-item label="重试次数">{{ currentDetail.retryCount }}</el-descriptions-item>
          <el-descriptions-item label="上报时间">{{ currentDetail.reportTime ? fmt(currentDetail.reportTime) : '—' }}</el-descriptions-item>
          <el-descriptions-item label="错误信息">{{ currentDetail.errorMessage || '无' }}</el-descriptions-item>
        </el-descriptions>
        <div style="margin-top: 16px">
          <h4>上报数据</h4>
          <el-input v-model="currentDetail.payload" type="textarea" :rows="10" readonly />
        </div>
      </div>
      <template #footer><el-button @click="detailDialog = false">关闭</el-button></template>
    </el-dialog>
  </div>
</template>

<script setup lang="ts">
import { reactive, ref, watch } from "vue";
import { ElMessage, ElMessageBox } from "element-plus";
import { http } from "@/api/http";

type PageResponse<T> = { list: T[]; page: number; pageSize: number; total: number };
type Warning = { id: number; externalId: string; companyCode: string | null; riskLevel: string | null; pushType: string | null; warningTime: number | null; rawJson: string };
type Feedback = { id: number; externalId: string; feedback: string; status: string; lastError: string | null };
type ReportLog = { id: number; reportType: string; businessId: string; status: string; retryCount: number; errorMessage: string | null; reportTime: number | null; payload: string };

const tab = ref("unitReport");

// 企业信息上报表单
const unitForm = reactive({
  unitCode: "53090200003",
  unitName: "",
  unitType: "",
  creditCode: "",
  legalPerson: "",
  contactPerson: "",
  contactPhone: "",
  address: "",
  longitude: 102.712251,
  latitude: 25.040609,
  areaCode: "530902",
  businessLicense: "",
  establishDate: "",
  registerCapital: "",
  unitStatus: "在营",
  hazardCategories: [] as string[],
  maxCapacity: "",
  emergencyPlan: "",
  emergencyContact: "",
  emergencyPhone: ""
});

const unitRules = {
  unitCode: [{ required: true, message: "请输入企业编码", trigger: "blur" }],
  unitName: [{ required: true, message: "请输入企业名称", trigger: "blur" }],
  unitType: [{ required: true, message: "请选择企业类型", trigger: "change" }],
  creditCode: [{ required: true, message: "请输入信用代码", trigger: "blur" }],
  legalPerson: [{ required: true, message: "请输入法定代表人", trigger: "blur" }],
  contactPerson: [{ required: true, message: "请输入联系人", trigger: "blur" }],
  contactPhone: [{ required: true, message: "请输入联系电话", trigger: "blur" }],
  address: [{ required: true, message: "请输入企业地址", trigger: "blur" }],
  areaCode: [{ required: true, message: "请输入行政区划代码", trigger: "blur" }]
};

const unitFormRef = ref();
const unitSubmitting = ref(false);

// 预警信息上报表单
const warningForm = reactive({
  warningId: "",
  unitCode: "53090200003",
  unitName: "",
  warningType: "",
  warningLevel: "",
  warningTime: Date.now(),
  warningContent: "",
  warningStatus: "未处理",
  handleResult: "",
  handlePerson: "",
  handleTime: undefined as number | undefined,
  feedback: "",
  longitude: 102.712251,
  latitude: 25.040609,
  deviceCode: "",
  deviceName: "",
  hazardCategory: "",
  capacity: "",
  actualAmount: "",
  stopType: "",
  stopReason: ""
});

const warningRules = {
  warningId: [{ required: true, message: "请输入预警ID", trigger: "blur" }],
  unitCode: [{ required: true, message: "请输入企业编码", trigger: "blur" }],
  unitName: [{ required: true, message: "请输入企业名称", trigger: "blur" }],
  warningType: [{ required: true, message: "请选择预警类型", trigger: "change" }],
  warningLevel: [{ required: true, message: "请选择预警等级", trigger: "change" }],
  warningTime: [{ required: true, message: "请选择预警时间", trigger: "change" }],
  warningContent: [{ required: true, message: "请输入预警内容", trigger: "blur" }]
};

const warningFormRef = ref();
const warningSubmitting = ref(false);

// 预警列表（从省厅拉取）
const loadingWarnings = ref(false);
const warnings = ref<Warning[]>([]);
const warningsTotal = ref(0);
const warningsPage = ref(1);
const warningsPageSize = ref(20);

// 反馈记录
const loadingFeedback = ref(false);
const feedback = ref<Feedback[]>([]);
const feedbackTotal = ref(0);
const feedbackPage = ref(1);
const feedbackPageSize = ref(20);

// 上报记录
const loadingLogs = ref(false);
const reportLogs = ref<ReportLog[]>([]);
const logsTotal = ref(0);
const logsPage = ref(1);
const logsPageSize = ref(20);
const logTypeFilter = ref("");
const logStatusFilter = ref("");

// 拉取反馈
const fetchWarningId = ref("");
const fetching = ref(false);
const fetchedFeedback = ref<any[]>([]);

// 对话框
const jsonDialog = ref(false);
const jsonText = ref("");
const detailDialog = ref(false);
const currentDetail = ref<ReportLog | null>(null);

function fmt(ts: number) {
  const d = new Date(ts);
  const pad = (n: number) => String(n).padStart(2, "0");
  return `${d.getFullYear()}-${pad(d.getMonth() + 1)}-${pad(d.getDate())} ${pad(d.getHours())}:${pad(d.getMinutes())}:${pad(d.getSeconds())}`;
}

function showJson(text: string) {
  jsonText.value = text || "";
  jsonDialog.value = true;
}

function showReportDetail(row: ReportLog) {
  currentDetail.value = row;
  detailDialog.value = true;
}

function handleReportUnit() {
  tab.value = "unitReport";
}

function handleReportWarning() {
  tab.value = "warningReport";
}

function resetUnitForm() {
  unitFormRef.value?.resetFields();
  unitForm.hazardCategories = [];
}

function resetWarningForm() {
  warningFormRef.value?.resetFields();
  warningForm.warningTime = Date.now();
}

async function submitUnitReport() {
  await unitFormRef.value?.validate(async (valid) => {
    if (!valid) return;
    unitSubmitting.value = true;
    try {
      await http.post<boolean>("/api/v1/provincial/report/unit", { ...unitForm });
      ElMessage.success("企业信息上报成功");
      resetUnitForm();
    } catch (e: any) {
      ElMessage.error(e?.message || "上报失败");
    } finally {
      unitSubmitting.value = false;
    }
  });
}

async function submitWarningReport() {
  await warningFormRef.value?.validate(async (valid) => {
    if (!valid) return;
    warningSubmitting.value = true;
    try {
      await http.post<boolean>("/api/v1/provincial/report/warning", { ...warningForm });
      ElMessage.success("预警信息上报成功");
      resetWarningForm();
    } catch (e: any) {
      ElMessage.error(e?.message || "上报失败");
    } finally {
      warningSubmitting.value = false;
    }
  });
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

async function loadReportLogs() {
  loadingLogs.value = true;
  try {
    const params = new URLSearchParams({
      page: String(logsPage.value),
      pageSize: String(logsPageSize.value)
    });
    if (logTypeFilter.value) params.set("type", logTypeFilter.value);
    if (logStatusFilter.value) params.set("status", logStatusFilter.value);
    // 假设有上报记录查询接口
    // const data = await http.get<PageResponse<ReportLog>>(`/api/v1/provincial/report/logs?${params}`);
    // reportLogs.value = data.list;
    // logsTotal.value = data.total;
    reportLogs.value = [];
    logsTotal.value = 0;
  } catch (e: any) {
    ElMessage.error(e?.message || "加载失败");
  } finally {
    loadingLogs.value = false;
  }
}

async function fetchFeedbackFromProvincial() {
  if (!fetchWarningId.value) {
    ElMessage.warning("请输入预警ID");
    return;
  }
  fetching.value = true;
  try {
    const data = await http.get<any[]>(`/api/v1/provincial/report/feedback/${fetchWarningId.value}`);
    fetchedFeedback.value = data;
    ElMessage.success(`拉取到 ${data.length} 条反馈记录`);
  } catch (e: any) {
    ElMessage.error(e?.message || "拉取失败");
  } finally {
    fetching.value = false;
  }
}

async function retryFeedback(externalId: string) {
  try {
    await http.post("/api/v1/provincial/feedback", { externalId, feedback: "重试反馈" });
    ElMessage.success("重试成功");
    await loadFeedback();
  } catch (e: any) {
    ElMessage.error(e?.message || "重试失败");
  }
}

async function retryReport(id: number) {
  try {
    await http.post(`/api/v1/provincial/report/retry/${id}`);
    ElMessage.success("重试成功");
    await loadReportLogs();
  } catch (e: any) {
    ElMessage.error(e?.message || "重试失败");
  }
}

async function loadAll() {
  await Promise.all([loadWarnings(), loadFeedback(), loadReportLogs()]);
}

watch([warningsPage, warningsPageSize], () => loadWarnings());
watch([feedbackPage, feedbackPageSize], () => loadFeedback());
watch([logsPage, logsPageSize], () => loadReportLogs());
watch(tab, (val) => {
  if (val === "warnings") loadWarnings();
  else if (val === "feedback") loadFeedback();
  else if (val === "reportLogs") loadReportLogs();
});

// 初始化加载
loadWarnings();
</script>

<style scoped>
.sf-form-section {
  padding: 20px;
}

.sf-form-section-title {
  font-size: 16px;
  font-weight: 600;
  margin-bottom: 16px;
  color: #333;
}

.sf-form-grid {
  display: grid;
  grid-template-columns: repeat(2, 1fr);
  gap: 12px 20px;
}

.sf-form-full {
  grid-column: 1 / -1;
}

.sf-toolbar {
  display: flex;
  gap: 12px;
  margin-bottom: 16px;
}
</style>
