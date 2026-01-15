import { computed, reactive, ref, watch } from "vue";
import { ElMessage } from "element-plus";
import { http } from "@/api/http";
const filters = reactive({
    channel: "",
    messageType: "",
    ok: null,
    apiKey: "",
    companyCode: "",
    topicLike: ""
});
const loading = ref(false);
const list = ref([]);
const total = ref(0);
const page = ref(1);
const pageSize = ref(20);
const payloadVisible = ref(false);
const payloadText = ref("");
const successCount = computed(() => list.value.filter((item) => item.parsedOk === 1).length);
const failCount = computed(() => list.value.filter((item) => item.parsedOk !== 1).length);
const failRate = computed(() => (list.value.length ? Math.round((failCount.value / list.value.length) * 100) : 0));
function formatTime(ms) {
    if (!ms)
        return "-";
    const d = new Date(ms);
    const pad = (n) => String(n).padStart(2, "0");
    return `${d.getFullYear()}-${pad(d.getMonth() + 1)}-${pad(d.getDate())} ${pad(d.getHours())}:${pad(d.getMinutes())}:${pad(d.getSeconds())}`;
}
function buildQuery() {
    const params = new URLSearchParams();
    params.set("page", String(page.value));
    params.set("pageSize", String(pageSize.value));
    if (filters.channel)
        params.set("channel", filters.channel);
    if (filters.messageType)
        params.set("messageType", filters.messageType);
    if (filters.ok !== null)
        params.set("ok", String(filters.ok));
    if (filters.apiKey)
        params.set("apiKey", filters.apiKey);
    if (filters.companyCode)
        params.set("companyCode", filters.companyCode);
    if (filters.topicLike)
        params.set("topicLike", filters.topicLike);
    return params.toString();
}
async function load() {
    loading.value = true;
    try {
        const data = await http.get(`/api/v1/hardware/ingest-logs?${buildQuery()}`);
        list.value = data.list;
        total.value = data.total;
    }
    catch (e) {
        ElMessage.error(e?.message || "加载失败");
    }
    finally {
        loading.value = false;
    }
}
function openPayload(row) {
    payloadText.value = row.payload || "";
    payloadVisible.value = true;
}
async function copyPayload() {
    try {
        await navigator.clipboard.writeText(payloadText.value || "");
        ElMessage.success("已复制");
    }
    catch {
        ElMessage.error("复制失败（浏览器权限限制）");
    }
}
watch([page, pageSize], () => load());
watch(() => ({ ...filters }), () => {
    page.value = 1;
});
load();
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
}));
const __VLS_2 = __VLS_1({
    ...{ 'onClick': {} },
}, ...__VLS_functionalComponentArgsRest(__VLS_1));
let __VLS_4;
let __VLS_5;
let __VLS_6;
const __VLS_7 = {
    onClick: (__VLS_ctx.load)
};
__VLS_3.slots.default;
var __VLS_3;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-section" },
    ...{ style: {} },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-filter" },
});
const __VLS_8 = {}.ElSelect;
/** @type {[typeof __VLS_components.ElSelect, typeof __VLS_components.elSelect, typeof __VLS_components.ElSelect, typeof __VLS_components.elSelect, ]} */ ;
// @ts-ignore
const __VLS_9 = __VLS_asFunctionalComponent(__VLS_8, new __VLS_8({
    modelValue: (__VLS_ctx.filters.channel),
    placeholder: "通道",
    clearable: true,
    ...{ style: {} },
}));
const __VLS_10 = __VLS_9({
    modelValue: (__VLS_ctx.filters.channel),
    placeholder: "通道",
    clearable: true,
    ...{ style: {} },
}, ...__VLS_functionalComponentArgsRest(__VLS_9));
__VLS_11.slots.default;
const __VLS_12 = {}.ElOption;
/** @type {[typeof __VLS_components.ElOption, typeof __VLS_components.elOption, ]} */ ;
// @ts-ignore
const __VLS_13 = __VLS_asFunctionalComponent(__VLS_12, new __VLS_12({
    label: "HTTP",
    value: "HTTP",
}));
const __VLS_14 = __VLS_13({
    label: "HTTP",
    value: "HTTP",
}, ...__VLS_functionalComponentArgsRest(__VLS_13));
var __VLS_11;
const __VLS_16 = {}.ElSelect;
/** @type {[typeof __VLS_components.ElSelect, typeof __VLS_components.elSelect, typeof __VLS_components.ElSelect, typeof __VLS_components.elSelect, ]} */ ;
// @ts-ignore
const __VLS_17 = __VLS_asFunctionalComponent(__VLS_16, new __VLS_16({
    modelValue: (__VLS_ctx.filters.messageType),
    placeholder: "类型",
    clearable: true,
    ...{ style: {} },
}));
const __VLS_18 = __VLS_17({
    modelValue: (__VLS_ctx.filters.messageType),
    placeholder: "类型",
    clearable: true,
    ...{ style: {} },
}, ...__VLS_functionalComponentArgsRest(__VLS_17));
__VLS_19.slots.default;
const __VLS_20 = {}.ElOption;
/** @type {[typeof __VLS_components.ElOption, typeof __VLS_components.elOption, ]} */ ;
// @ts-ignore
const __VLS_21 = __VLS_asFunctionalComponent(__VLS_20, new __VLS_20({
    label: "device-reading（实时值）",
    value: "device-reading",
}));
const __VLS_22 = __VLS_21({
    label: "device-reading（实时值）",
    value: "device-reading",
}, ...__VLS_functionalComponentArgsRest(__VLS_21));
const __VLS_24 = {}.ElOption;
/** @type {[typeof __VLS_components.ElOption, typeof __VLS_components.elOption, ]} */ ;
// @ts-ignore
const __VLS_25 = __VLS_asFunctionalComponent(__VLS_24, new __VLS_24({
    label: "alarm（告警）",
    value: "alarm",
}));
const __VLS_26 = __VLS_25({
    label: "alarm（告警）",
    value: "alarm",
}, ...__VLS_functionalComponentArgsRest(__VLS_25));
const __VLS_28 = {}.ElOption;
/** @type {[typeof __VLS_components.ElOption, typeof __VLS_components.elOption, ]} */ ;
// @ts-ignore
const __VLS_29 = __VLS_asFunctionalComponent(__VLS_28, new __VLS_28({
    label: "person-inout（人员出入）",
    value: "person-inout",
}));
const __VLS_30 = __VLS_29({
    label: "person-inout（人员出入）",
    value: "person-inout",
}, ...__VLS_functionalComponentArgsRest(__VLS_29));
const __VLS_32 = {}.ElOption;
/** @type {[typeof __VLS_components.ElOption, typeof __VLS_components.elOption, ]} */ ;
// @ts-ignore
const __VLS_33 = __VLS_asFunctionalComponent(__VLS_32, new __VLS_32({
    label: "car-inout（车辆出入）",
    value: "car-inout",
}));
const __VLS_34 = __VLS_33({
    label: "car-inout（车辆出入）",
    value: "car-inout",
}, ...__VLS_functionalComponentArgsRest(__VLS_33));
const __VLS_36 = {}.ElOption;
/** @type {[typeof __VLS_components.ElOption, typeof __VLS_components.elOption, ]} */ ;
// @ts-ignore
const __VLS_37 = __VLS_asFunctionalComponent(__VLS_36, new __VLS_36({
    label: "server-heartbeat（服务器心跳）",
    value: "server-heartbeat",
}));
const __VLS_38 = __VLS_37({
    label: "server-heartbeat（服务器心跳）",
    value: "server-heartbeat",
}, ...__VLS_functionalComponentArgsRest(__VLS_37));
var __VLS_19;
const __VLS_40 = {}.ElSelect;
/** @type {[typeof __VLS_components.ElSelect, typeof __VLS_components.elSelect, typeof __VLS_components.ElSelect, typeof __VLS_components.elSelect, ]} */ ;
// @ts-ignore
const __VLS_41 = __VLS_asFunctionalComponent(__VLS_40, new __VLS_40({
    modelValue: (__VLS_ctx.filters.ok),
    placeholder: "结果",
    clearable: true,
    ...{ style: {} },
}));
const __VLS_42 = __VLS_41({
    modelValue: (__VLS_ctx.filters.ok),
    placeholder: "结果",
    clearable: true,
    ...{ style: {} },
}, ...__VLS_functionalComponentArgsRest(__VLS_41));
__VLS_43.slots.default;
const __VLS_44 = {}.ElOption;
/** @type {[typeof __VLS_components.ElOption, typeof __VLS_components.elOption, ]} */ ;
// @ts-ignore
const __VLS_45 = __VLS_asFunctionalComponent(__VLS_44, new __VLS_44({
    label: "成功",
    value: (1),
}));
const __VLS_46 = __VLS_45({
    label: "成功",
    value: (1),
}, ...__VLS_functionalComponentArgsRest(__VLS_45));
const __VLS_48 = {}.ElOption;
/** @type {[typeof __VLS_components.ElOption, typeof __VLS_components.elOption, ]} */ ;
// @ts-ignore
const __VLS_49 = __VLS_asFunctionalComponent(__VLS_48, new __VLS_48({
    label: "失败",
    value: (0),
}));
const __VLS_50 = __VLS_49({
    label: "失败",
    value: (0),
}, ...__VLS_functionalComponentArgsRest(__VLS_49));
var __VLS_43;
const __VLS_52 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_53 = __VLS_asFunctionalComponent(__VLS_52, new __VLS_52({
    modelValue: (__VLS_ctx.filters.companyCode),
    placeholder: "企业编码",
    ...{ style: {} },
    clearable: true,
}));
const __VLS_54 = __VLS_53({
    modelValue: (__VLS_ctx.filters.companyCode),
    placeholder: "企业编码",
    ...{ style: {} },
    clearable: true,
}, ...__VLS_functionalComponentArgsRest(__VLS_53));
const __VLS_56 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_57 = __VLS_asFunctionalComponent(__VLS_56, new __VLS_56({
    modelValue: (__VLS_ctx.filters.apiKey),
    placeholder: "apiKey",
    ...{ style: {} },
    clearable: true,
}));
const __VLS_58 = __VLS_57({
    modelValue: (__VLS_ctx.filters.apiKey),
    placeholder: "apiKey",
    ...{ style: {} },
    clearable: true,
}, ...__VLS_functionalComponentArgsRest(__VLS_57));
const __VLS_60 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_61 = __VLS_asFunctionalComponent(__VLS_60, new __VLS_60({
    modelValue: (__VLS_ctx.filters.topicLike),
    placeholder: "topic/path 包含",
    ...{ style: {} },
    clearable: true,
}));
const __VLS_62 = __VLS_61({
    modelValue: (__VLS_ctx.filters.topicLike),
    placeholder: "topic/path 包含",
    ...{ style: {} },
    clearable: true,
}, ...__VLS_functionalComponentArgsRest(__VLS_61));
const __VLS_64 = {}.ElButton;
/** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
// @ts-ignore
const __VLS_65 = __VLS_asFunctionalComponent(__VLS_64, new __VLS_64({
    ...{ 'onClick': {} },
}));
const __VLS_66 = __VLS_65({
    ...{ 'onClick': {} },
}, ...__VLS_functionalComponentArgsRest(__VLS_65));
let __VLS_68;
let __VLS_69;
let __VLS_70;
const __VLS_71 = {
    onClick: (__VLS_ctx.load)
};
__VLS_67.slots.default;
var __VLS_67;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-kpi sf-stagger" },
    ...{ style: {} },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-card sf-kpi-item" },
    ...{ style: {} },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-kpi-title" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-kpi-value" },
});
(__VLS_ctx.list.length);
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-card sf-kpi-item" },
    ...{ style: {} },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-kpi-title" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-kpi-value" },
    ...{ style: {} },
});
(__VLS_ctx.successCount);
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-card sf-kpi-item" },
    ...{ style: {} },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-kpi-title" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-kpi-value" },
    ...{ style: {} },
});
(__VLS_ctx.failCount);
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-card sf-kpi-item" },
    ...{ style: {} },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-kpi-title" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-kpi-value" },
});
(__VLS_ctx.failRate);
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-card sf-table-card" },
});
const __VLS_72 = {}.ElTable;
/** @type {[typeof __VLS_components.ElTable, typeof __VLS_components.elTable, typeof __VLS_components.ElTable, typeof __VLS_components.elTable, ]} */ ;
// @ts-ignore
const __VLS_73 = __VLS_asFunctionalComponent(__VLS_72, new __VLS_72({
    data: (__VLS_ctx.list),
    ...{ style: {} },
}));
const __VLS_74 = __VLS_73({
    data: (__VLS_ctx.list),
    ...{ style: {} },
}, ...__VLS_functionalComponentArgsRest(__VLS_73));
__VLS_asFunctionalDirective(__VLS_directives.vLoading)(null, { ...__VLS_directiveBindingRestFields, value: (__VLS_ctx.loading) }, null, null);
__VLS_75.slots.default;
const __VLS_76 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_77 = __VLS_asFunctionalComponent(__VLS_76, new __VLS_76({
    prop: "id",
    label: "ID",
    width: "90",
}));
const __VLS_78 = __VLS_77({
    prop: "id",
    label: "ID",
    width: "90",
}, ...__VLS_functionalComponentArgsRest(__VLS_77));
const __VLS_80 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_81 = __VLS_asFunctionalComponent(__VLS_80, new __VLS_80({
    prop: "receiveTimeMs",
    label: "时间",
    width: "170",
}));
const __VLS_82 = __VLS_81({
    prop: "receiveTimeMs",
    label: "时间",
    width: "170",
}, ...__VLS_functionalComponentArgsRest(__VLS_81));
__VLS_83.slots.default;
{
    const { default: __VLS_thisSlot } = __VLS_83.slots;
    const [{ row }] = __VLS_getSlotParams(__VLS_thisSlot);
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({
        ...{ class: "sf-muted" },
    });
    (__VLS_ctx.formatTime(row.receiveTimeMs));
}
var __VLS_83;
const __VLS_84 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_85 = __VLS_asFunctionalComponent(__VLS_84, new __VLS_84({
    prop: "ingestChannel",
    label: "通道",
    width: "90",
}));
const __VLS_86 = __VLS_85({
    prop: "ingestChannel",
    label: "通道",
    width: "90",
}, ...__VLS_functionalComponentArgsRest(__VLS_85));
const __VLS_88 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_89 = __VLS_asFunctionalComponent(__VLS_88, new __VLS_88({
    prop: "messageType",
    label: "类型",
    width: "160",
}));
const __VLS_90 = __VLS_89({
    prop: "messageType",
    label: "类型",
    width: "160",
}, ...__VLS_functionalComponentArgsRest(__VLS_89));
__VLS_91.slots.default;
{
    const { default: __VLS_thisSlot } = __VLS_91.slots;
    const [{ row }] = __VLS_getSlotParams(__VLS_thisSlot);
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({
        ...{ class: "sf-chip" },
    });
    (row.messageType);
}
var __VLS_91;
const __VLS_92 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_93 = __VLS_asFunctionalComponent(__VLS_92, new __VLS_92({
    prop: "companyCode",
    label: "企业",
    width: "110",
}));
const __VLS_94 = __VLS_93({
    prop: "companyCode",
    label: "企业",
    width: "110",
}, ...__VLS_functionalComponentArgsRest(__VLS_93));
__VLS_95.slots.default;
{
    const { default: __VLS_thisSlot } = __VLS_95.slots;
    const [{ row }] = __VLS_getSlotParams(__VLS_thisSlot);
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({
        ...{ class: "sf-chip" },
    });
    (row.companyCode || "-");
}
var __VLS_95;
const __VLS_96 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_97 = __VLS_asFunctionalComponent(__VLS_96, new __VLS_96({
    prop: "apiKey",
    label: "apiKey",
    minWidth: "140",
}));
const __VLS_98 = __VLS_97({
    prop: "apiKey",
    label: "apiKey",
    minWidth: "140",
}, ...__VLS_functionalComponentArgsRest(__VLS_97));
const __VLS_100 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_101 = __VLS_asFunctionalComponent(__VLS_100, new __VLS_100({
    prop: "topic",
    label: "topic/path",
    minWidth: "200",
    showOverflowTooltip: true,
}));
const __VLS_102 = __VLS_101({
    prop: "topic",
    label: "topic/path",
    minWidth: "200",
    showOverflowTooltip: true,
}, ...__VLS_functionalComponentArgsRest(__VLS_101));
const __VLS_104 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_105 = __VLS_asFunctionalComponent(__VLS_104, new __VLS_104({
    prop: "parsedOk",
    label: "结果",
    width: "90",
}));
const __VLS_106 = __VLS_105({
    prop: "parsedOk",
    label: "结果",
    width: "90",
}, ...__VLS_functionalComponentArgsRest(__VLS_105));
__VLS_107.slots.default;
{
    const { default: __VLS_thisSlot } = __VLS_107.slots;
    const [{ row }] = __VLS_getSlotParams(__VLS_thisSlot);
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({
        ...{ class: "sf-chip" },
        ...{ style: (row.parsedOk === 1 ? 'border-color: rgba(51,209,122,0.25)' : 'border-color: rgba(255,107,107,0.22)') },
    });
    (row.parsedOk === 1 ? "成功" : "失败");
}
var __VLS_107;
const __VLS_108 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_109 = __VLS_asFunctionalComponent(__VLS_108, new __VLS_108({
    prop: "errorMessage",
    label: "失败原因",
    minWidth: "180",
    showOverflowTooltip: true,
}));
const __VLS_110 = __VLS_109({
    prop: "errorMessage",
    label: "失败原因",
    minWidth: "180",
    showOverflowTooltip: true,
}, ...__VLS_functionalComponentArgsRest(__VLS_109));
const __VLS_112 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_113 = __VLS_asFunctionalComponent(__VLS_112, new __VLS_112({
    label: "操作",
    width: "120",
    fixed: "right",
}));
const __VLS_114 = __VLS_113({
    label: "操作",
    width: "120",
    fixed: "right",
}, ...__VLS_functionalComponentArgsRest(__VLS_113));
__VLS_115.slots.default;
{
    const { default: __VLS_thisSlot } = __VLS_115.slots;
    const [{ row }] = __VLS_getSlotParams(__VLS_thisSlot);
    const __VLS_116 = {}.ElButton;
    /** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
    // @ts-ignore
    const __VLS_117 = __VLS_asFunctionalComponent(__VLS_116, new __VLS_116({
        ...{ 'onClick': {} },
        size: "small",
    }));
    const __VLS_118 = __VLS_117({
        ...{ 'onClick': {} },
        size: "small",
    }, ...__VLS_functionalComponentArgsRest(__VLS_117));
    let __VLS_120;
    let __VLS_121;
    let __VLS_122;
    const __VLS_123 = {
        onClick: (...[$event]) => {
            __VLS_ctx.openPayload(row);
        }
    };
    __VLS_119.slots.default;
    var __VLS_119;
}
var __VLS_115;
var __VLS_75;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ style: {} },
});
const __VLS_124 = {}.ElPagination;
/** @type {[typeof __VLS_components.ElPagination, typeof __VLS_components.elPagination, ]} */ ;
// @ts-ignore
const __VLS_125 = __VLS_asFunctionalComponent(__VLS_124, new __VLS_124({
    ...{ 'onChange': {} },
    background: true,
    layout: "prev, pager, next, sizes, total",
    total: (__VLS_ctx.total),
    currentPage: (__VLS_ctx.page),
    pageSize: (__VLS_ctx.pageSize),
}));
const __VLS_126 = __VLS_125({
    ...{ 'onChange': {} },
    background: true,
    layout: "prev, pager, next, sizes, total",
    total: (__VLS_ctx.total),
    currentPage: (__VLS_ctx.page),
    pageSize: (__VLS_ctx.pageSize),
}, ...__VLS_functionalComponentArgsRest(__VLS_125));
let __VLS_128;
let __VLS_129;
let __VLS_130;
const __VLS_131 = {
    onChange: (__VLS_ctx.load)
};
var __VLS_127;
const __VLS_132 = {}.ElDialog;
/** @type {[typeof __VLS_components.ElDialog, typeof __VLS_components.elDialog, typeof __VLS_components.ElDialog, typeof __VLS_components.elDialog, ]} */ ;
// @ts-ignore
const __VLS_133 = __VLS_asFunctionalComponent(__VLS_132, new __VLS_132({
    modelValue: (__VLS_ctx.payloadVisible),
    title: "原始上报数据",
    width: "860px",
}));
const __VLS_134 = __VLS_133({
    modelValue: (__VLS_ctx.payloadVisible),
    title: "原始上报数据",
    width: "860px",
}, ...__VLS_functionalComponentArgsRest(__VLS_133));
__VLS_135.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ style: {} },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-muted" },
});
const __VLS_136 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_137 = __VLS_asFunctionalComponent(__VLS_136, new __VLS_136({
    modelValue: (__VLS_ctx.payloadText),
    type: "textarea",
    rows: (16),
    readonly: true,
}));
const __VLS_138 = __VLS_137({
    modelValue: (__VLS_ctx.payloadText),
    type: "textarea",
    rows: (16),
    readonly: true,
}, ...__VLS_functionalComponentArgsRest(__VLS_137));
{
    const { footer: __VLS_thisSlot } = __VLS_135.slots;
    const __VLS_140 = {}.ElButton;
    /** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
    // @ts-ignore
    const __VLS_141 = __VLS_asFunctionalComponent(__VLS_140, new __VLS_140({
        ...{ 'onClick': {} },
    }));
    const __VLS_142 = __VLS_141({
        ...{ 'onClick': {} },
    }, ...__VLS_functionalComponentArgsRest(__VLS_141));
    let __VLS_144;
    let __VLS_145;
    let __VLS_146;
    const __VLS_147 = {
        onClick: (...[$event]) => {
            __VLS_ctx.payloadVisible = false;
        }
    };
    __VLS_143.slots.default;
    var __VLS_143;
    const __VLS_148 = {}.ElButton;
    /** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
    // @ts-ignore
    const __VLS_149 = __VLS_asFunctionalComponent(__VLS_148, new __VLS_148({
        ...{ 'onClick': {} },
        type: "primary",
    }));
    const __VLS_150 = __VLS_149({
        ...{ 'onClick': {} },
        type: "primary",
    }, ...__VLS_functionalComponentArgsRest(__VLS_149));
    let __VLS_152;
    let __VLS_153;
    let __VLS_154;
    const __VLS_155 = {
        onClick: (__VLS_ctx.copyPayload)
    };
    __VLS_151.slots.default;
    var __VLS_151;
}
var __VLS_135;
/** @type {__VLS_StyleScopedClasses['sf-page']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-page-head']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-page-title']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-page-sub']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-page-actions']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-section']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-filter']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-kpi']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-stagger']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-card']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-kpi-item']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-kpi-title']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-kpi-value']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-card']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-kpi-item']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-kpi-title']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-kpi-value']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-card']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-kpi-item']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-kpi-title']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-kpi-value']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-card']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-kpi-item']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-kpi-title']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-kpi-value']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-card']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-table-card']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-muted']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-chip']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-chip']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-chip']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-muted']} */ ;
var __VLS_dollars;
const __VLS_self = (await import('vue')).defineComponent({
    setup() {
        return {
            filters: filters,
            loading: loading,
            list: list,
            total: total,
            page: page,
            pageSize: pageSize,
            payloadVisible: payloadVisible,
            payloadText: payloadText,
            successCount: successCount,
            failCount: failCount,
            failRate: failRate,
            formatTime: formatTime,
            load: load,
            openPayload: openPayload,
            copyPayload: copyPayload,
        };
    },
});
export default (await import('vue')).defineComponent({
    setup() {
        return {};
    },
});
; /* PartiallyEnd: #4569/main.vue */
