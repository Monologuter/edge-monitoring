import { reactive, ref, watch } from "vue";
import { ElMessage } from "element-plus";
import { http } from "@/api/http";
const tab = ref("warnings");
const loadingWarnings = ref(false);
const warnings = ref([]);
const warningsTotal = ref(0);
const warningsPage = ref(1);
const warningsPageSize = ref(20);
const loadingFeedback = ref(false);
const feedback = ref([]);
const feedbackTotal = ref(0);
const feedbackPage = ref(1);
const feedbackPageSize = ref(20);
const feedbackDialog = ref(false);
const saving = ref(false);
const feedbackForm = reactive({ externalId: "", feedback: "" });
const jsonDialog = ref(false);
const jsonText = ref("");
function fmt(ts) {
    const d = new Date(ts);
    const pad = (n) => String(n).padStart(2, "0");
    return `${d.getFullYear()}-${pad(d.getMonth() + 1)}-${pad(d.getDate())} ${pad(d.getHours())}:${pad(d.getMinutes())}:${pad(d.getSeconds())}`;
}
function showJson(text) {
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
        const data = await http.get(`/api/v1/provincial/warnings?page=${warningsPage.value}&pageSize=${warningsPageSize.value}`);
        warnings.value = data.list;
        warningsTotal.value = data.total;
    }
    catch (e) {
        ElMessage.error(e?.message || "加载失败");
    }
    finally {
        loadingWarnings.value = false;
    }
}
async function loadFeedback() {
    loadingFeedback.value = true;
    try {
        const data = await http.get(`/api/v1/provincial/feedback?page=${feedbackPage.value}&pageSize=${feedbackPageSize.value}`);
        feedback.value = data.list;
        feedbackTotal.value = data.total;
    }
    catch (e) {
        ElMessage.error(e?.message || "加载失败");
    }
    finally {
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
        await http.post("/api/v1/provincial/feedback", { ...feedbackForm });
        ElMessage.success("提交成功");
        feedbackDialog.value = false;
        tab.value = "feedback";
        await loadFeedback();
    }
    catch (e) {
        ElMessage.error(e?.message || "提交失败");
    }
    finally {
        saving.value = false;
    }
}
async function loadAll() {
    await Promise.all([loadWarnings(), loadFeedback()]);
}
watch([warningsPage, warningsPageSize], () => loadWarnings());
watch([feedbackPage, feedbackPageSize], () => loadFeedback());
watch(tab, () => {
    if (tab.value === "warnings")
        loadWarnings();
    else
        loadFeedback();
});
loadWarnings();
debugger; /* PartiallyEnd: #3632/scriptSetup.vue */
const __VLS_ctx = {};
let __VLS_components;
let __VLS_directives;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-page" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-page-head" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
__VLS_asFunctionalElement(__VLS_intrinsicElements.h2, __VLS_intrinsicElements.h2)({
    ...{ class: "sf-page-title" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-page-sub" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-page-actions" },
});
const __VLS_0 = {}.ElButton;
/** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
// @ts-ignore
const __VLS_1 = __VLS_asFunctionalComponent(__VLS_0, new __VLS_0({
    ...{ 'onClick': {} },
    type: "primary",
}));
const __VLS_2 = __VLS_1({
    ...{ 'onClick': {} },
    type: "primary",
}, ...__VLS_functionalComponentArgsRest(__VLS_1));
let __VLS_4;
let __VLS_5;
let __VLS_6;
const __VLS_7 = {
    onClick: (__VLS_ctx.openFeedback)
};
__VLS_3.slots.default;
var __VLS_3;
const __VLS_8 = {}.ElButton;
/** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
// @ts-ignore
const __VLS_9 = __VLS_asFunctionalComponent(__VLS_8, new __VLS_8({
    ...{ 'onClick': {} },
}));
const __VLS_10 = __VLS_9({
    ...{ 'onClick': {} },
}, ...__VLS_functionalComponentArgsRest(__VLS_9));
let __VLS_12;
let __VLS_13;
let __VLS_14;
const __VLS_15 = {
    onClick: (__VLS_ctx.loadAll)
};
__VLS_11.slots.default;
var __VLS_11;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-card sf-table-card" },
});
const __VLS_16 = {}.ElTabs;
/** @type {[typeof __VLS_components.ElTabs, typeof __VLS_components.elTabs, typeof __VLS_components.ElTabs, typeof __VLS_components.elTabs, ]} */ ;
// @ts-ignore
const __VLS_17 = __VLS_asFunctionalComponent(__VLS_16, new __VLS_16({
    modelValue: (__VLS_ctx.tab),
}));
const __VLS_18 = __VLS_17({
    modelValue: (__VLS_ctx.tab),
}, ...__VLS_functionalComponentArgsRest(__VLS_17));
__VLS_19.slots.default;
const __VLS_20 = {}.ElTabPane;
/** @type {[typeof __VLS_components.ElTabPane, typeof __VLS_components.elTabPane, typeof __VLS_components.ElTabPane, typeof __VLS_components.elTabPane, ]} */ ;
// @ts-ignore
const __VLS_21 = __VLS_asFunctionalComponent(__VLS_20, new __VLS_20({
    label: "预警列表",
    name: "warnings",
}));
const __VLS_22 = __VLS_21({
    label: "预警列表",
    name: "warnings",
}, ...__VLS_functionalComponentArgsRest(__VLS_21));
__VLS_23.slots.default;
const __VLS_24 = {}.ElTable;
/** @type {[typeof __VLS_components.ElTable, typeof __VLS_components.elTable, typeof __VLS_components.ElTable, typeof __VLS_components.elTable, ]} */ ;
// @ts-ignore
const __VLS_25 = __VLS_asFunctionalComponent(__VLS_24, new __VLS_24({
    data: (__VLS_ctx.warnings),
    ...{ style: {} },
}));
const __VLS_26 = __VLS_25({
    data: (__VLS_ctx.warnings),
    ...{ style: {} },
}, ...__VLS_functionalComponentArgsRest(__VLS_25));
__VLS_asFunctionalDirective(__VLS_directives.vLoading)(null, { ...__VLS_directiveBindingRestFields, value: (__VLS_ctx.loadingWarnings) }, null, null);
__VLS_27.slots.default;
const __VLS_28 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_29 = __VLS_asFunctionalComponent(__VLS_28, new __VLS_28({
    prop: "externalId",
    label: "预警ID",
    minWidth: "160",
}));
const __VLS_30 = __VLS_29({
    prop: "externalId",
    label: "预警ID",
    minWidth: "160",
}, ...__VLS_functionalComponentArgsRest(__VLS_29));
const __VLS_32 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_33 = __VLS_asFunctionalComponent(__VLS_32, new __VLS_32({
    prop: "riskLevel",
    label: "风险等级",
    width: "120",
}));
const __VLS_34 = __VLS_33({
    prop: "riskLevel",
    label: "风险等级",
    width: "120",
}, ...__VLS_functionalComponentArgsRest(__VLS_33));
const __VLS_36 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_37 = __VLS_asFunctionalComponent(__VLS_36, new __VLS_36({
    prop: "pushType",
    label: "推送类型",
    width: "120",
}));
const __VLS_38 = __VLS_37({
    prop: "pushType",
    label: "推送类型",
    width: "120",
}, ...__VLS_functionalComponentArgsRest(__VLS_37));
const __VLS_40 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_41 = __VLS_asFunctionalComponent(__VLS_40, new __VLS_40({
    prop: "warningTime",
    label: "时间",
    minWidth: "180",
}));
const __VLS_42 = __VLS_41({
    prop: "warningTime",
    label: "时间",
    minWidth: "180",
}, ...__VLS_functionalComponentArgsRest(__VLS_41));
__VLS_43.slots.default;
{
    const { default: __VLS_thisSlot } = __VLS_43.slots;
    const [{ row }] = __VLS_getSlotParams(__VLS_thisSlot);
    (row.warningTime ? __VLS_ctx.fmt(row.warningTime) : "—");
}
var __VLS_43;
const __VLS_44 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_45 = __VLS_asFunctionalComponent(__VLS_44, new __VLS_44({
    prop: "companyCode",
    label: "企业编码",
    minWidth: "140",
}));
const __VLS_46 = __VLS_45({
    prop: "companyCode",
    label: "企业编码",
    minWidth: "140",
}, ...__VLS_functionalComponentArgsRest(__VLS_45));
const __VLS_48 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_49 = __VLS_asFunctionalComponent(__VLS_48, new __VLS_48({
    label: "原始报文",
    width: "120",
}));
const __VLS_50 = __VLS_49({
    label: "原始报文",
    width: "120",
}, ...__VLS_functionalComponentArgsRest(__VLS_49));
__VLS_51.slots.default;
{
    const { default: __VLS_thisSlot } = __VLS_51.slots;
    const [{ row }] = __VLS_getSlotParams(__VLS_thisSlot);
    const __VLS_52 = {}.ElButton;
    /** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
    // @ts-ignore
    const __VLS_53 = __VLS_asFunctionalComponent(__VLS_52, new __VLS_52({
        ...{ 'onClick': {} },
        size: "small",
    }));
    const __VLS_54 = __VLS_53({
        ...{ 'onClick': {} },
        size: "small",
    }, ...__VLS_functionalComponentArgsRest(__VLS_53));
    let __VLS_56;
    let __VLS_57;
    let __VLS_58;
    const __VLS_59 = {
        onClick: (...[$event]) => {
            __VLS_ctx.showJson(row.rawJson);
        }
    };
    __VLS_55.slots.default;
    var __VLS_55;
}
var __VLS_51;
var __VLS_27;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ style: {} },
});
const __VLS_60 = {}.ElPagination;
/** @type {[typeof __VLS_components.ElPagination, typeof __VLS_components.elPagination, ]} */ ;
// @ts-ignore
const __VLS_61 = __VLS_asFunctionalComponent(__VLS_60, new __VLS_60({
    ...{ 'onChange': {} },
    background: true,
    layout: "prev, pager, next, sizes, total",
    total: (__VLS_ctx.warningsTotal),
    currentPage: (__VLS_ctx.warningsPage),
    pageSize: (__VLS_ctx.warningsPageSize),
}));
const __VLS_62 = __VLS_61({
    ...{ 'onChange': {} },
    background: true,
    layout: "prev, pager, next, sizes, total",
    total: (__VLS_ctx.warningsTotal),
    currentPage: (__VLS_ctx.warningsPage),
    pageSize: (__VLS_ctx.warningsPageSize),
}, ...__VLS_functionalComponentArgsRest(__VLS_61));
let __VLS_64;
let __VLS_65;
let __VLS_66;
const __VLS_67 = {
    onChange: (__VLS_ctx.loadWarnings)
};
var __VLS_63;
var __VLS_23;
const __VLS_68 = {}.ElTabPane;
/** @type {[typeof __VLS_components.ElTabPane, typeof __VLS_components.elTabPane, typeof __VLS_components.ElTabPane, typeof __VLS_components.elTabPane, ]} */ ;
// @ts-ignore
const __VLS_69 = __VLS_asFunctionalComponent(__VLS_68, new __VLS_68({
    label: "反馈记录",
    name: "feedback",
}));
const __VLS_70 = __VLS_69({
    label: "反馈记录",
    name: "feedback",
}, ...__VLS_functionalComponentArgsRest(__VLS_69));
__VLS_71.slots.default;
const __VLS_72 = {}.ElTable;
/** @type {[typeof __VLS_components.ElTable, typeof __VLS_components.elTable, typeof __VLS_components.ElTable, typeof __VLS_components.elTable, ]} */ ;
// @ts-ignore
const __VLS_73 = __VLS_asFunctionalComponent(__VLS_72, new __VLS_72({
    data: (__VLS_ctx.feedback),
    ...{ style: {} },
}));
const __VLS_74 = __VLS_73({
    data: (__VLS_ctx.feedback),
    ...{ style: {} },
}, ...__VLS_functionalComponentArgsRest(__VLS_73));
__VLS_asFunctionalDirective(__VLS_directives.vLoading)(null, { ...__VLS_directiveBindingRestFields, value: (__VLS_ctx.loadingFeedback) }, null, null);
__VLS_75.slots.default;
const __VLS_76 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_77 = __VLS_asFunctionalComponent(__VLS_76, new __VLS_76({
    prop: "externalId",
    label: "预警ID",
    minWidth: "160",
}));
const __VLS_78 = __VLS_77({
    prop: "externalId",
    label: "预警ID",
    minWidth: "160",
}, ...__VLS_functionalComponentArgsRest(__VLS_77));
const __VLS_80 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_81 = __VLS_asFunctionalComponent(__VLS_80, new __VLS_80({
    prop: "feedback",
    label: "反馈",
    minWidth: "260",
}));
const __VLS_82 = __VLS_81({
    prop: "feedback",
    label: "反馈",
    minWidth: "260",
}, ...__VLS_functionalComponentArgsRest(__VLS_81));
const __VLS_84 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_85 = __VLS_asFunctionalComponent(__VLS_84, new __VLS_84({
    prop: "status",
    label: "状态",
    width: "120",
}));
const __VLS_86 = __VLS_85({
    prop: "status",
    label: "状态",
    width: "120",
}, ...__VLS_functionalComponentArgsRest(__VLS_85));
const __VLS_88 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_89 = __VLS_asFunctionalComponent(__VLS_88, new __VLS_88({
    prop: "lastError",
    label: "错误",
    minWidth: "220",
}));
const __VLS_90 = __VLS_89({
    prop: "lastError",
    label: "错误",
    minWidth: "220",
}, ...__VLS_functionalComponentArgsRest(__VLS_89));
var __VLS_75;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ style: {} },
});
const __VLS_92 = {}.ElPagination;
/** @type {[typeof __VLS_components.ElPagination, typeof __VLS_components.elPagination, ]} */ ;
// @ts-ignore
const __VLS_93 = __VLS_asFunctionalComponent(__VLS_92, new __VLS_92({
    ...{ 'onChange': {} },
    background: true,
    layout: "prev, pager, next, sizes, total",
    total: (__VLS_ctx.feedbackTotal),
    currentPage: (__VLS_ctx.feedbackPage),
    pageSize: (__VLS_ctx.feedbackPageSize),
}));
const __VLS_94 = __VLS_93({
    ...{ 'onChange': {} },
    background: true,
    layout: "prev, pager, next, sizes, total",
    total: (__VLS_ctx.feedbackTotal),
    currentPage: (__VLS_ctx.feedbackPage),
    pageSize: (__VLS_ctx.feedbackPageSize),
}, ...__VLS_functionalComponentArgsRest(__VLS_93));
let __VLS_96;
let __VLS_97;
let __VLS_98;
const __VLS_99 = {
    onChange: (__VLS_ctx.loadFeedback)
};
var __VLS_95;
var __VLS_71;
var __VLS_19;
const __VLS_100 = {}.ElDialog;
/** @type {[typeof __VLS_components.ElDialog, typeof __VLS_components.elDialog, typeof __VLS_components.ElDialog, typeof __VLS_components.elDialog, ]} */ ;
// @ts-ignore
const __VLS_101 = __VLS_asFunctionalComponent(__VLS_100, new __VLS_100({
    modelValue: (__VLS_ctx.feedbackDialog),
    title: "提交省厅预警反馈",
    width: "560px",
}));
const __VLS_102 = __VLS_101({
    modelValue: (__VLS_ctx.feedbackDialog),
    title: "提交省厅预警反馈",
    width: "560px",
}, ...__VLS_functionalComponentArgsRest(__VLS_101));
__VLS_103.slots.default;
const __VLS_104 = {}.ElForm;
/** @type {[typeof __VLS_components.ElForm, typeof __VLS_components.elForm, typeof __VLS_components.ElForm, typeof __VLS_components.elForm, ]} */ ;
// @ts-ignore
const __VLS_105 = __VLS_asFunctionalComponent(__VLS_104, new __VLS_104({
    labelWidth: "90px",
}));
const __VLS_106 = __VLS_105({
    labelWidth: "90px",
}, ...__VLS_functionalComponentArgsRest(__VLS_105));
__VLS_107.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-form-grid" },
});
const __VLS_108 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_109 = __VLS_asFunctionalComponent(__VLS_108, new __VLS_108({
    label: "预警ID",
    required: true,
}));
const __VLS_110 = __VLS_109({
    label: "预警ID",
    required: true,
}, ...__VLS_functionalComponentArgsRest(__VLS_109));
__VLS_111.slots.default;
const __VLS_112 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_113 = __VLS_asFunctionalComponent(__VLS_112, new __VLS_112({
    modelValue: (__VLS_ctx.feedbackForm.externalId),
}));
const __VLS_114 = __VLS_113({
    modelValue: (__VLS_ctx.feedbackForm.externalId),
}, ...__VLS_functionalComponentArgsRest(__VLS_113));
var __VLS_111;
const __VLS_116 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_117 = __VLS_asFunctionalComponent(__VLS_116, new __VLS_116({
    label: "反馈内容",
    required: true,
    ...{ class: "sf-form-full" },
}));
const __VLS_118 = __VLS_117({
    label: "反馈内容",
    required: true,
    ...{ class: "sf-form-full" },
}, ...__VLS_functionalComponentArgsRest(__VLS_117));
__VLS_119.slots.default;
const __VLS_120 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_121 = __VLS_asFunctionalComponent(__VLS_120, new __VLS_120({
    modelValue: (__VLS_ctx.feedbackForm.feedback),
    type: "textarea",
    rows: (3),
}));
const __VLS_122 = __VLS_121({
    modelValue: (__VLS_ctx.feedbackForm.feedback),
    type: "textarea",
    rows: (3),
}, ...__VLS_functionalComponentArgsRest(__VLS_121));
var __VLS_119;
var __VLS_107;
{
    const { footer: __VLS_thisSlot } = __VLS_103.slots;
    const __VLS_124 = {}.ElButton;
    /** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
    // @ts-ignore
    const __VLS_125 = __VLS_asFunctionalComponent(__VLS_124, new __VLS_124({
        ...{ 'onClick': {} },
    }));
    const __VLS_126 = __VLS_125({
        ...{ 'onClick': {} },
    }, ...__VLS_functionalComponentArgsRest(__VLS_125));
    let __VLS_128;
    let __VLS_129;
    let __VLS_130;
    const __VLS_131 = {
        onClick: (...[$event]) => {
            __VLS_ctx.feedbackDialog = false;
        }
    };
    __VLS_127.slots.default;
    var __VLS_127;
    const __VLS_132 = {}.ElButton;
    /** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
    // @ts-ignore
    const __VLS_133 = __VLS_asFunctionalComponent(__VLS_132, new __VLS_132({
        ...{ 'onClick': {} },
        type: "primary",
        loading: (__VLS_ctx.saving),
    }));
    const __VLS_134 = __VLS_133({
        ...{ 'onClick': {} },
        type: "primary",
        loading: (__VLS_ctx.saving),
    }, ...__VLS_functionalComponentArgsRest(__VLS_133));
    let __VLS_136;
    let __VLS_137;
    let __VLS_138;
    const __VLS_139 = {
        onClick: (__VLS_ctx.submitFeedback)
    };
    __VLS_135.slots.default;
    var __VLS_135;
}
var __VLS_103;
const __VLS_140 = {}.ElDialog;
/** @type {[typeof __VLS_components.ElDialog, typeof __VLS_components.elDialog, typeof __VLS_components.ElDialog, typeof __VLS_components.elDialog, ]} */ ;
// @ts-ignore
const __VLS_141 = __VLS_asFunctionalComponent(__VLS_140, new __VLS_140({
    modelValue: (__VLS_ctx.jsonDialog),
    title: "原始报文",
    width: "760px",
}));
const __VLS_142 = __VLS_141({
    modelValue: (__VLS_ctx.jsonDialog),
    title: "原始报文",
    width: "760px",
}, ...__VLS_functionalComponentArgsRest(__VLS_141));
__VLS_143.slots.default;
const __VLS_144 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_145 = __VLS_asFunctionalComponent(__VLS_144, new __VLS_144({
    modelValue: (__VLS_ctx.jsonText),
    type: "textarea",
    rows: (16),
}));
const __VLS_146 = __VLS_145({
    modelValue: (__VLS_ctx.jsonText),
    type: "textarea",
    rows: (16),
}, ...__VLS_functionalComponentArgsRest(__VLS_145));
{
    const { footer: __VLS_thisSlot } = __VLS_143.slots;
    const __VLS_148 = {}.ElButton;
    /** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
    // @ts-ignore
    const __VLS_149 = __VLS_asFunctionalComponent(__VLS_148, new __VLS_148({
        ...{ 'onClick': {} },
    }));
    const __VLS_150 = __VLS_149({
        ...{ 'onClick': {} },
    }, ...__VLS_functionalComponentArgsRest(__VLS_149));
    let __VLS_152;
    let __VLS_153;
    let __VLS_154;
    const __VLS_155 = {
        onClick: (...[$event]) => {
            __VLS_ctx.jsonDialog = false;
        }
    };
    __VLS_151.slots.default;
    var __VLS_151;
}
var __VLS_143;
/** @type {__VLS_StyleScopedClasses['sf-page']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-page-head']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-page-title']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-page-sub']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-page-actions']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-card']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-table-card']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-form-grid']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-form-full']} */ ;
var __VLS_dollars;
const __VLS_self = (await import('vue')).defineComponent({
    setup() {
        return {
            tab: tab,
            loadingWarnings: loadingWarnings,
            warnings: warnings,
            warningsTotal: warningsTotal,
            warningsPage: warningsPage,
            warningsPageSize: warningsPageSize,
            loadingFeedback: loadingFeedback,
            feedback: feedback,
            feedbackTotal: feedbackTotal,
            feedbackPage: feedbackPage,
            feedbackPageSize: feedbackPageSize,
            feedbackDialog: feedbackDialog,
            saving: saving,
            feedbackForm: feedbackForm,
            jsonDialog: jsonDialog,
            jsonText: jsonText,
            fmt: fmt,
            showJson: showJson,
            openFeedback: openFeedback,
            loadWarnings: loadWarnings,
            loadFeedback: loadFeedback,
            submitFeedback: submitFeedback,
            loadAll: loadAll,
        };
    },
});
export default (await import('vue')).defineComponent({
    setup() {
        return {};
    },
});
; /* PartiallyEnd: #4569/main.vue */
