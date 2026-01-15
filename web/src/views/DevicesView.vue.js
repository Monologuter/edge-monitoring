import { nextTick, reactive, ref, watch } from "vue";
import { ElMessage } from "element-plus";
import { http } from "@/api/http";
import * as echarts from "echarts";
const loading = ref(false);
const list = ref([]);
const total = ref(0);
const page = ref(1);
const pageSize = ref(20);
const deviceTypeFilter = ref(null);
const deviceTypeOptions = [
    { label: "视频", value: 1 },
    { label: "红外", value: 2 },
    { label: "温度", value: 3 },
    { label: "湿度", value: 4 },
    { label: "液位", value: 5 }
];
function deviceTypeLabel(value) {
    const found = deviceTypeOptions.find((item) => item.value === value);
    return found ? found.label : "未知";
}
const dialogVisible = ref(false);
const saving = ref(false);
const editing = ref(null);
const form = reactive({
    companyCode: "",
    deviceCode: "",
    deviceName: "",
    deviceType: 3,
    unit: "",
    lowerLimit: null,
    upperLimit: null,
    locationName: "",
    storeNum: "",
    storeroomNum: "",
    ipAddress: "",
    accessUsername: "",
    accessPassword: "",
    onlineStatus: 0
});
const trendVisible = ref(false);
const chartEl = ref(null);
let chart = null;
async function load() {
    loading.value = true;
    try {
        const data = await http.get(`/api/v1/devices?page=${page.value}&pageSize=${pageSize.value}${deviceTypeFilter.value ? `&deviceType=${deviceTypeFilter.value}` : ""}`);
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
function openCreate() {
    editing.value = null;
    Object.assign(form, {
        companyCode: "",
        deviceCode: "",
        deviceName: "",
        deviceType: 3,
        unit: "",
        lowerLimit: null,
        upperLimit: null,
        locationName: "",
        storeNum: "",
        storeroomNum: "",
        ipAddress: "",
        accessUsername: "",
        accessPassword: "",
        onlineStatus: 0
    });
    dialogVisible.value = true;
}
function openEdit(row) {
    editing.value = row;
    Object.assign(form, {
        companyCode: row.companyCode || "",
        deviceCode: row.deviceCode,
        deviceName: row.deviceName,
        deviceType: row.deviceType,
        unit: row.unit || "",
        lowerLimit: row.lowerLimit,
        upperLimit: row.upperLimit,
        locationName: row.locationName || "",
        storeNum: row.storeNum || "",
        storeroomNum: row.storeroomNum || "",
        ipAddress: row.ipAddress || "",
        accessUsername: row.accessUsername || "",
        accessPassword: row.accessPassword || "",
        onlineStatus: row.onlineStatus
    });
    dialogVisible.value = true;
}
async function onSave() {
    if (!form.deviceCode || !form.deviceName) {
        ElMessage.warning("请填写设备编码与设备名称");
        return;
    }
    saving.value = true;
    try {
        if (!editing.value?.id) {
            await http.post("/api/v1/devices", { ...form });
        }
        else {
            await http.put("/api/v1/devices", { id: editing.value.id, ...form });
        }
        ElMessage.success("保存成功");
        dialogVisible.value = false;
        await load();
    }
    catch (e) {
        ElMessage.error(e?.message || "保存失败");
    }
    finally {
        saving.value = false;
    }
}
async function onDelete(id) {
    try {
        await http.del(`/api/v1/devices/${id}`);
        ElMessage.success("删除成功");
        await load();
    }
    catch (e) {
        ElMessage.error(e?.message || "删除失败");
    }
}
async function openTrend(deviceCode) {
    trendVisible.value = true;
    await nextTick();
    try {
        const data = await http.get(`/api/v1/readings/hourly?deviceCode=${encodeURIComponent(deviceCode)}&hours=24`);
        if (!chartEl.value)
            return;
        chart?.dispose();
        chart = echarts.init(chartEl.value);
        chart.setOption({
            backgroundColor: "transparent",
            grid: { left: 40, right: 18, top: 22, bottom: 34 },
            tooltip: { trigger: "axis" },
            xAxis: {
                type: "category",
                data: data.map((x) => new Date(x.hourStart).getHours().toString().padStart(2, "0") + ":00"),
                axisLabel: { color: "rgba(255,255,255,0.7)" },
                axisLine: { lineStyle: { color: "rgba(255,255,255,0.14)" } }
            },
            yAxis: {
                type: "value",
                axisLabel: { color: "rgba(255,255,255,0.7)" },
                splitLine: { lineStyle: { color: "rgba(255,255,255,0.08)" } }
            },
            series: [
                {
                    name: "平均值",
                    type: "line",
                    smooth: true,
                    data: data.map((x) => x.avgValue),
                    lineStyle: { width: 3, color: "rgba(61,214,198,0.95)" },
                    symbol: "circle",
                    symbolSize: 7
                }
            ]
        });
    }
    catch (e) {
        ElMessage.error(e?.message || "加载趋势失败");
    }
}
watch([page, pageSize], () => load());
watch(deviceTypeFilter, () => {
    page.value = 1;
    load();
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
const __VLS_0 = {}.ElSelect;
/** @type {[typeof __VLS_components.ElSelect, typeof __VLS_components.elSelect, typeof __VLS_components.ElSelect, typeof __VLS_components.elSelect, ]} */ ;
// @ts-ignore
const __VLS_1 = __VLS_asFunctionalComponent(__VLS_0, new __VLS_0({
    modelValue: (__VLS_ctx.deviceTypeFilter),
    placeholder: "设备类型",
    ...{ style: {} },
    clearable: true,
}));
const __VLS_2 = __VLS_1({
    modelValue: (__VLS_ctx.deviceTypeFilter),
    placeholder: "设备类型",
    ...{ style: {} },
    clearable: true,
}, ...__VLS_functionalComponentArgsRest(__VLS_1));
__VLS_3.slots.default;
for (const [t] of __VLS_getVForSourceType((__VLS_ctx.deviceTypeOptions))) {
    const __VLS_4 = {}.ElOption;
    /** @type {[typeof __VLS_components.ElOption, typeof __VLS_components.elOption, ]} */ ;
    // @ts-ignore
    const __VLS_5 = __VLS_asFunctionalComponent(__VLS_4, new __VLS_4({
        key: (t.value),
        label: (t.label),
        value: (t.value),
    }));
    const __VLS_6 = __VLS_5({
        key: (t.value),
        label: (t.label),
        value: (t.value),
    }, ...__VLS_functionalComponentArgsRest(__VLS_5));
}
var __VLS_3;
const __VLS_8 = {}.ElButton;
/** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
// @ts-ignore
const __VLS_9 = __VLS_asFunctionalComponent(__VLS_8, new __VLS_8({
    ...{ 'onClick': {} },
    type: "primary",
}));
const __VLS_10 = __VLS_9({
    ...{ 'onClick': {} },
    type: "primary",
}, ...__VLS_functionalComponentArgsRest(__VLS_9));
let __VLS_12;
let __VLS_13;
let __VLS_14;
const __VLS_15 = {
    onClick: (__VLS_ctx.openCreate)
};
__VLS_11.slots.default;
var __VLS_11;
const __VLS_16 = {}.ElButton;
/** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
// @ts-ignore
const __VLS_17 = __VLS_asFunctionalComponent(__VLS_16, new __VLS_16({
    ...{ 'onClick': {} },
}));
const __VLS_18 = __VLS_17({
    ...{ 'onClick': {} },
}, ...__VLS_functionalComponentArgsRest(__VLS_17));
let __VLS_20;
let __VLS_21;
let __VLS_22;
const __VLS_23 = {
    onClick: (__VLS_ctx.load)
};
__VLS_19.slots.default;
var __VLS_19;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-card sf-table-card" },
});
const __VLS_24 = {}.ElTable;
/** @type {[typeof __VLS_components.ElTable, typeof __VLS_components.elTable, typeof __VLS_components.ElTable, typeof __VLS_components.elTable, ]} */ ;
// @ts-ignore
const __VLS_25 = __VLS_asFunctionalComponent(__VLS_24, new __VLS_24({
    data: (__VLS_ctx.list),
    ...{ style: {} },
}));
const __VLS_26 = __VLS_25({
    data: (__VLS_ctx.list),
    ...{ style: {} },
}, ...__VLS_functionalComponentArgsRest(__VLS_25));
__VLS_asFunctionalDirective(__VLS_directives.vLoading)(null, { ...__VLS_directiveBindingRestFields, value: (__VLS_ctx.loading) }, null, null);
__VLS_27.slots.default;
const __VLS_28 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_29 = __VLS_asFunctionalComponent(__VLS_28, new __VLS_28({
    prop: "companyCode",
    label: "企业编码",
    minWidth: "140",
}));
const __VLS_30 = __VLS_29({
    prop: "companyCode",
    label: "企业编码",
    minWidth: "140",
}, ...__VLS_functionalComponentArgsRest(__VLS_29));
const __VLS_32 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_33 = __VLS_asFunctionalComponent(__VLS_32, new __VLS_32({
    prop: "deviceCode",
    label: "编码",
    minWidth: "140",
}));
const __VLS_34 = __VLS_33({
    prop: "deviceCode",
    label: "编码",
    minWidth: "140",
}, ...__VLS_functionalComponentArgsRest(__VLS_33));
const __VLS_36 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_37 = __VLS_asFunctionalComponent(__VLS_36, new __VLS_36({
    prop: "deviceName",
    label: "名称",
    minWidth: "180",
}));
const __VLS_38 = __VLS_37({
    prop: "deviceName",
    label: "名称",
    minWidth: "180",
}, ...__VLS_functionalComponentArgsRest(__VLS_37));
const __VLS_40 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_41 = __VLS_asFunctionalComponent(__VLS_40, new __VLS_40({
    prop: "deviceType",
    label: "类型",
    width: "110",
}));
const __VLS_42 = __VLS_41({
    prop: "deviceType",
    label: "类型",
    width: "110",
}, ...__VLS_functionalComponentArgsRest(__VLS_41));
__VLS_43.slots.default;
{
    const { default: __VLS_thisSlot } = __VLS_43.slots;
    const [{ row }] = __VLS_getSlotParams(__VLS_thisSlot);
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({
        ...{ class: "sf-chip" },
    });
    (__VLS_ctx.deviceTypeLabel(row.deviceType));
}
var __VLS_43;
const __VLS_44 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_45 = __VLS_asFunctionalComponent(__VLS_44, new __VLS_44({
    prop: "locationName",
    label: "位置",
    minWidth: "160",
}));
const __VLS_46 = __VLS_45({
    prop: "locationName",
    label: "位置",
    minWidth: "160",
}, ...__VLS_functionalComponentArgsRest(__VLS_45));
const __VLS_48 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_49 = __VLS_asFunctionalComponent(__VLS_48, new __VLS_48({
    prop: "storeNum",
    label: "仓库编码",
    minWidth: "140",
}));
const __VLS_50 = __VLS_49({
    prop: "storeNum",
    label: "仓库编码",
    minWidth: "140",
}, ...__VLS_functionalComponentArgsRest(__VLS_49));
const __VLS_52 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_53 = __VLS_asFunctionalComponent(__VLS_52, new __VLS_52({
    prop: "storeroomNum",
    label: "库房编码",
    minWidth: "140",
}));
const __VLS_54 = __VLS_53({
    prop: "storeroomNum",
    label: "库房编码",
    minWidth: "140",
}, ...__VLS_functionalComponentArgsRest(__VLS_53));
const __VLS_56 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_57 = __VLS_asFunctionalComponent(__VLS_56, new __VLS_56({
    prop: "ipAddress",
    label: "IP地址",
    minWidth: "140",
}));
const __VLS_58 = __VLS_57({
    prop: "ipAddress",
    label: "IP地址",
    minWidth: "140",
}, ...__VLS_functionalComponentArgsRest(__VLS_57));
const __VLS_60 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_61 = __VLS_asFunctionalComponent(__VLS_60, new __VLS_60({
    prop: "accessUsername",
    label: "账号",
    width: "120",
}));
const __VLS_62 = __VLS_61({
    prop: "accessUsername",
    label: "账号",
    width: "120",
}, ...__VLS_functionalComponentArgsRest(__VLS_61));
__VLS_63.slots.default;
{
    const { default: __VLS_thisSlot } = __VLS_63.slots;
    const [{ row }] = __VLS_getSlotParams(__VLS_thisSlot);
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({
        ...{ class: "sf-muted" },
    });
    (row.accessUsername ? "••••••" : "-");
}
var __VLS_63;
const __VLS_64 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_65 = __VLS_asFunctionalComponent(__VLS_64, new __VLS_64({
    prop: "accessPassword",
    label: "密码",
    width: "120",
}));
const __VLS_66 = __VLS_65({
    prop: "accessPassword",
    label: "密码",
    width: "120",
}, ...__VLS_functionalComponentArgsRest(__VLS_65));
__VLS_67.slots.default;
{
    const { default: __VLS_thisSlot } = __VLS_67.slots;
    const [{ row }] = __VLS_getSlotParams(__VLS_thisSlot);
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({
        ...{ class: "sf-muted" },
    });
    (row.accessPassword ? "••••••" : "-");
}
var __VLS_67;
const __VLS_68 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_69 = __VLS_asFunctionalComponent(__VLS_68, new __VLS_68({
    label: "阈值",
    minWidth: "160",
}));
const __VLS_70 = __VLS_69({
    label: "阈值",
    minWidth: "160",
}, ...__VLS_functionalComponentArgsRest(__VLS_69));
__VLS_71.slots.default;
{
    const { default: __VLS_thisSlot } = __VLS_71.slots;
    const [{ row }] = __VLS_getSlotParams(__VLS_thisSlot);
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({
        ...{ class: "sf-muted" },
    });
    (row.lowerLimit ?? "-");
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({
        ...{ class: "sf-muted" },
    });
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({
        ...{ class: "sf-muted" },
    });
    (row.upperLimit ?? "-");
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({
        ...{ class: "sf-muted" },
    });
    (row.unit || "");
}
var __VLS_71;
const __VLS_72 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_73 = __VLS_asFunctionalComponent(__VLS_72, new __VLS_72({
    prop: "onlineStatus",
    label: "在线",
    width: "90",
}));
const __VLS_74 = __VLS_73({
    prop: "onlineStatus",
    label: "在线",
    width: "90",
}, ...__VLS_functionalComponentArgsRest(__VLS_73));
__VLS_75.slots.default;
{
    const { default: __VLS_thisSlot } = __VLS_75.slots;
    const [{ row }] = __VLS_getSlotParams(__VLS_thisSlot);
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({
        ...{ class: "sf-chip" },
        ...{ style: (row.onlineStatus === 1 ? 'border-color: rgba(51,209,122,0.25)' : 'border-color: rgba(255,255,255,0.12)') },
    });
    (row.onlineStatus === 1 ? "在线" : "离线");
}
var __VLS_75;
const __VLS_76 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_77 = __VLS_asFunctionalComponent(__VLS_76, new __VLS_76({
    label: "操作",
    width: "210",
    fixed: "right",
}));
const __VLS_78 = __VLS_77({
    label: "操作",
    width: "210",
    fixed: "right",
}, ...__VLS_functionalComponentArgsRest(__VLS_77));
__VLS_79.slots.default;
{
    const { default: __VLS_thisSlot } = __VLS_79.slots;
    const [{ row }] = __VLS_getSlotParams(__VLS_thisSlot);
    const __VLS_80 = {}.ElButton;
    /** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
    // @ts-ignore
    const __VLS_81 = __VLS_asFunctionalComponent(__VLS_80, new __VLS_80({
        ...{ 'onClick': {} },
        size: "small",
    }));
    const __VLS_82 = __VLS_81({
        ...{ 'onClick': {} },
        size: "small",
    }, ...__VLS_functionalComponentArgsRest(__VLS_81));
    let __VLS_84;
    let __VLS_85;
    let __VLS_86;
    const __VLS_87 = {
        onClick: (...[$event]) => {
            __VLS_ctx.openTrend(row.deviceCode);
        }
    };
    __VLS_83.slots.default;
    var __VLS_83;
    const __VLS_88 = {}.ElButton;
    /** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
    // @ts-ignore
    const __VLS_89 = __VLS_asFunctionalComponent(__VLS_88, new __VLS_88({
        ...{ 'onClick': {} },
        size: "small",
    }));
    const __VLS_90 = __VLS_89({
        ...{ 'onClick': {} },
        size: "small",
    }, ...__VLS_functionalComponentArgsRest(__VLS_89));
    let __VLS_92;
    let __VLS_93;
    let __VLS_94;
    const __VLS_95 = {
        onClick: (...[$event]) => {
            __VLS_ctx.openEdit(row);
        }
    };
    __VLS_91.slots.default;
    var __VLS_91;
    const __VLS_96 = {}.ElButton;
    /** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
    // @ts-ignore
    const __VLS_97 = __VLS_asFunctionalComponent(__VLS_96, new __VLS_96({
        ...{ 'onClick': {} },
        size: "small",
        type: "danger",
        plain: true,
    }));
    const __VLS_98 = __VLS_97({
        ...{ 'onClick': {} },
        size: "small",
        type: "danger",
        plain: true,
    }, ...__VLS_functionalComponentArgsRest(__VLS_97));
    let __VLS_100;
    let __VLS_101;
    let __VLS_102;
    const __VLS_103 = {
        onClick: (...[$event]) => {
            __VLS_ctx.onDelete(row.id);
        }
    };
    __VLS_99.slots.default;
    var __VLS_99;
}
var __VLS_79;
var __VLS_27;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ style: {} },
});
const __VLS_104 = {}.ElPagination;
/** @type {[typeof __VLS_components.ElPagination, typeof __VLS_components.elPagination, ]} */ ;
// @ts-ignore
const __VLS_105 = __VLS_asFunctionalComponent(__VLS_104, new __VLS_104({
    ...{ 'onChange': {} },
    background: true,
    layout: "prev, pager, next, sizes, total",
    total: (__VLS_ctx.total),
    currentPage: (__VLS_ctx.page),
    pageSize: (__VLS_ctx.pageSize),
}));
const __VLS_106 = __VLS_105({
    ...{ 'onChange': {} },
    background: true,
    layout: "prev, pager, next, sizes, total",
    total: (__VLS_ctx.total),
    currentPage: (__VLS_ctx.page),
    pageSize: (__VLS_ctx.pageSize),
}, ...__VLS_functionalComponentArgsRest(__VLS_105));
let __VLS_108;
let __VLS_109;
let __VLS_110;
const __VLS_111 = {
    onChange: (__VLS_ctx.load)
};
var __VLS_107;
const __VLS_112 = {}.ElDialog;
/** @type {[typeof __VLS_components.ElDialog, typeof __VLS_components.elDialog, typeof __VLS_components.ElDialog, typeof __VLS_components.elDialog, ]} */ ;
// @ts-ignore
const __VLS_113 = __VLS_asFunctionalComponent(__VLS_112, new __VLS_112({
    modelValue: (__VLS_ctx.dialogVisible),
    title: (__VLS_ctx.editing?.id ? '编辑设备' : '新增设备'),
    width: "720px",
}));
const __VLS_114 = __VLS_113({
    modelValue: (__VLS_ctx.dialogVisible),
    title: (__VLS_ctx.editing?.id ? '编辑设备' : '新增设备'),
    width: "720px",
}, ...__VLS_functionalComponentArgsRest(__VLS_113));
__VLS_115.slots.default;
const __VLS_116 = {}.ElForm;
/** @type {[typeof __VLS_components.ElForm, typeof __VLS_components.elForm, typeof __VLS_components.ElForm, typeof __VLS_components.elForm, ]} */ ;
// @ts-ignore
const __VLS_117 = __VLS_asFunctionalComponent(__VLS_116, new __VLS_116({
    labelWidth: "110px",
}));
const __VLS_118 = __VLS_117({
    labelWidth: "110px",
}, ...__VLS_functionalComponentArgsRest(__VLS_117));
__VLS_119.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-form-grid" },
});
const __VLS_120 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_121 = __VLS_asFunctionalComponent(__VLS_120, new __VLS_120({
    label: "企业编码",
}));
const __VLS_122 = __VLS_121({
    label: "企业编码",
}, ...__VLS_functionalComponentArgsRest(__VLS_121));
__VLS_123.slots.default;
const __VLS_124 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_125 = __VLS_asFunctionalComponent(__VLS_124, new __VLS_124({
    modelValue: (__VLS_ctx.form.companyCode),
}));
const __VLS_126 = __VLS_125({
    modelValue: (__VLS_ctx.form.companyCode),
}, ...__VLS_functionalComponentArgsRest(__VLS_125));
var __VLS_123;
const __VLS_128 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_129 = __VLS_asFunctionalComponent(__VLS_128, new __VLS_128({
    label: "设备编码",
    required: true,
}));
const __VLS_130 = __VLS_129({
    label: "设备编码",
    required: true,
}, ...__VLS_functionalComponentArgsRest(__VLS_129));
__VLS_131.slots.default;
const __VLS_132 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_133 = __VLS_asFunctionalComponent(__VLS_132, new __VLS_132({
    modelValue: (__VLS_ctx.form.deviceCode),
}));
const __VLS_134 = __VLS_133({
    modelValue: (__VLS_ctx.form.deviceCode),
}, ...__VLS_functionalComponentArgsRest(__VLS_133));
var __VLS_131;
const __VLS_136 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_137 = __VLS_asFunctionalComponent(__VLS_136, new __VLS_136({
    label: "设备名称",
    required: true,
}));
const __VLS_138 = __VLS_137({
    label: "设备名称",
    required: true,
}, ...__VLS_functionalComponentArgsRest(__VLS_137));
__VLS_139.slots.default;
const __VLS_140 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_141 = __VLS_asFunctionalComponent(__VLS_140, new __VLS_140({
    modelValue: (__VLS_ctx.form.deviceName),
}));
const __VLS_142 = __VLS_141({
    modelValue: (__VLS_ctx.form.deviceName),
}, ...__VLS_functionalComponentArgsRest(__VLS_141));
var __VLS_139;
const __VLS_144 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_145 = __VLS_asFunctionalComponent(__VLS_144, new __VLS_144({
    label: "设备类型",
    required: true,
}));
const __VLS_146 = __VLS_145({
    label: "设备类型",
    required: true,
}, ...__VLS_functionalComponentArgsRest(__VLS_145));
__VLS_147.slots.default;
const __VLS_148 = {}.ElSelect;
/** @type {[typeof __VLS_components.ElSelect, typeof __VLS_components.elSelect, typeof __VLS_components.ElSelect, typeof __VLS_components.elSelect, ]} */ ;
// @ts-ignore
const __VLS_149 = __VLS_asFunctionalComponent(__VLS_148, new __VLS_148({
    modelValue: (__VLS_ctx.form.deviceType),
    ...{ style: {} },
    placeholder: "选择类型",
}));
const __VLS_150 = __VLS_149({
    modelValue: (__VLS_ctx.form.deviceType),
    ...{ style: {} },
    placeholder: "选择类型",
}, ...__VLS_functionalComponentArgsRest(__VLS_149));
__VLS_151.slots.default;
for (const [t] of __VLS_getVForSourceType((__VLS_ctx.deviceTypeOptions))) {
    const __VLS_152 = {}.ElOption;
    /** @type {[typeof __VLS_components.ElOption, typeof __VLS_components.elOption, ]} */ ;
    // @ts-ignore
    const __VLS_153 = __VLS_asFunctionalComponent(__VLS_152, new __VLS_152({
        key: (t.value),
        label: (t.label),
        value: (t.value),
    }));
    const __VLS_154 = __VLS_153({
        key: (t.value),
        label: (t.label),
        value: (t.value),
    }, ...__VLS_functionalComponentArgsRest(__VLS_153));
}
var __VLS_151;
var __VLS_147;
const __VLS_156 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_157 = __VLS_asFunctionalComponent(__VLS_156, new __VLS_156({
    label: "单位",
}));
const __VLS_158 = __VLS_157({
    label: "单位",
}, ...__VLS_functionalComponentArgsRest(__VLS_157));
__VLS_159.slots.default;
const __VLS_160 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_161 = __VLS_asFunctionalComponent(__VLS_160, new __VLS_160({
    modelValue: (__VLS_ctx.form.unit),
}));
const __VLS_162 = __VLS_161({
    modelValue: (__VLS_ctx.form.unit),
}, ...__VLS_functionalComponentArgsRest(__VLS_161));
var __VLS_159;
const __VLS_164 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_165 = __VLS_asFunctionalComponent(__VLS_164, new __VLS_164({
    label: "下限",
}));
const __VLS_166 = __VLS_165({
    label: "下限",
}, ...__VLS_functionalComponentArgsRest(__VLS_165));
__VLS_167.slots.default;
const __VLS_168 = {}.ElInputNumber;
/** @type {[typeof __VLS_components.ElInputNumber, typeof __VLS_components.elInputNumber, ]} */ ;
// @ts-ignore
const __VLS_169 = __VLS_asFunctionalComponent(__VLS_168, new __VLS_168({
    modelValue: (__VLS_ctx.form.lowerLimit),
    ...{ style: {} },
}));
const __VLS_170 = __VLS_169({
    modelValue: (__VLS_ctx.form.lowerLimit),
    ...{ style: {} },
}, ...__VLS_functionalComponentArgsRest(__VLS_169));
var __VLS_167;
const __VLS_172 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_173 = __VLS_asFunctionalComponent(__VLS_172, new __VLS_172({
    label: "上限",
}));
const __VLS_174 = __VLS_173({
    label: "上限",
}, ...__VLS_functionalComponentArgsRest(__VLS_173));
__VLS_175.slots.default;
const __VLS_176 = {}.ElInputNumber;
/** @type {[typeof __VLS_components.ElInputNumber, typeof __VLS_components.elInputNumber, ]} */ ;
// @ts-ignore
const __VLS_177 = __VLS_asFunctionalComponent(__VLS_176, new __VLS_176({
    modelValue: (__VLS_ctx.form.upperLimit),
    ...{ style: {} },
}));
const __VLS_178 = __VLS_177({
    modelValue: (__VLS_ctx.form.upperLimit),
    ...{ style: {} },
}, ...__VLS_functionalComponentArgsRest(__VLS_177));
var __VLS_175;
const __VLS_180 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_181 = __VLS_asFunctionalComponent(__VLS_180, new __VLS_180({
    label: "位置",
}));
const __VLS_182 = __VLS_181({
    label: "位置",
}, ...__VLS_functionalComponentArgsRest(__VLS_181));
__VLS_183.slots.default;
const __VLS_184 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_185 = __VLS_asFunctionalComponent(__VLS_184, new __VLS_184({
    modelValue: (__VLS_ctx.form.locationName),
}));
const __VLS_186 = __VLS_185({
    modelValue: (__VLS_ctx.form.locationName),
}, ...__VLS_functionalComponentArgsRest(__VLS_185));
var __VLS_183;
const __VLS_188 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_189 = __VLS_asFunctionalComponent(__VLS_188, new __VLS_188({
    label: "仓库编码",
}));
const __VLS_190 = __VLS_189({
    label: "仓库编码",
}, ...__VLS_functionalComponentArgsRest(__VLS_189));
__VLS_191.slots.default;
const __VLS_192 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_193 = __VLS_asFunctionalComponent(__VLS_192, new __VLS_192({
    modelValue: (__VLS_ctx.form.storeNum),
}));
const __VLS_194 = __VLS_193({
    modelValue: (__VLS_ctx.form.storeNum),
}, ...__VLS_functionalComponentArgsRest(__VLS_193));
var __VLS_191;
const __VLS_196 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_197 = __VLS_asFunctionalComponent(__VLS_196, new __VLS_196({
    label: "库房编码",
}));
const __VLS_198 = __VLS_197({
    label: "库房编码",
}, ...__VLS_functionalComponentArgsRest(__VLS_197));
__VLS_199.slots.default;
const __VLS_200 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_201 = __VLS_asFunctionalComponent(__VLS_200, new __VLS_200({
    modelValue: (__VLS_ctx.form.storeroomNum),
}));
const __VLS_202 = __VLS_201({
    modelValue: (__VLS_ctx.form.storeroomNum),
}, ...__VLS_functionalComponentArgsRest(__VLS_201));
var __VLS_199;
const __VLS_204 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_205 = __VLS_asFunctionalComponent(__VLS_204, new __VLS_204({
    label: "IP地址",
}));
const __VLS_206 = __VLS_205({
    label: "IP地址",
}, ...__VLS_functionalComponentArgsRest(__VLS_205));
__VLS_207.slots.default;
const __VLS_208 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_209 = __VLS_asFunctionalComponent(__VLS_208, new __VLS_208({
    modelValue: (__VLS_ctx.form.ipAddress),
}));
const __VLS_210 = __VLS_209({
    modelValue: (__VLS_ctx.form.ipAddress),
}, ...__VLS_functionalComponentArgsRest(__VLS_209));
var __VLS_207;
const __VLS_212 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_213 = __VLS_asFunctionalComponent(__VLS_212, new __VLS_212({
    label: "账号",
}));
const __VLS_214 = __VLS_213({
    label: "账号",
}, ...__VLS_functionalComponentArgsRest(__VLS_213));
__VLS_215.slots.default;
const __VLS_216 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_217 = __VLS_asFunctionalComponent(__VLS_216, new __VLS_216({
    modelValue: (__VLS_ctx.form.accessUsername),
}));
const __VLS_218 = __VLS_217({
    modelValue: (__VLS_ctx.form.accessUsername),
}, ...__VLS_functionalComponentArgsRest(__VLS_217));
var __VLS_215;
const __VLS_220 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_221 = __VLS_asFunctionalComponent(__VLS_220, new __VLS_220({
    label: "密码",
}));
const __VLS_222 = __VLS_221({
    label: "密码",
}, ...__VLS_functionalComponentArgsRest(__VLS_221));
__VLS_223.slots.default;
const __VLS_224 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_225 = __VLS_asFunctionalComponent(__VLS_224, new __VLS_224({
    modelValue: (__VLS_ctx.form.accessPassword),
    showPassword: true,
}));
const __VLS_226 = __VLS_225({
    modelValue: (__VLS_ctx.form.accessPassword),
    showPassword: true,
}, ...__VLS_functionalComponentArgsRest(__VLS_225));
var __VLS_223;
if (__VLS_ctx.editing?.id) {
    const __VLS_228 = {}.ElFormItem;
    /** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
    // @ts-ignore
    const __VLS_229 = __VLS_asFunctionalComponent(__VLS_228, new __VLS_228({
        label: "在线状态",
    }));
    const __VLS_230 = __VLS_229({
        label: "在线状态",
    }, ...__VLS_functionalComponentArgsRest(__VLS_229));
    __VLS_231.slots.default;
    const __VLS_232 = {}.ElSwitch;
    /** @type {[typeof __VLS_components.ElSwitch, typeof __VLS_components.elSwitch, ]} */ ;
    // @ts-ignore
    const __VLS_233 = __VLS_asFunctionalComponent(__VLS_232, new __VLS_232({
        modelValue: (__VLS_ctx.form.onlineStatus),
        activeValue: (1),
        inactiveValue: (0),
    }));
    const __VLS_234 = __VLS_233({
        modelValue: (__VLS_ctx.form.onlineStatus),
        activeValue: (1),
        inactiveValue: (0),
    }, ...__VLS_functionalComponentArgsRest(__VLS_233));
    var __VLS_231;
}
var __VLS_119;
{
    const { footer: __VLS_thisSlot } = __VLS_115.slots;
    const __VLS_236 = {}.ElButton;
    /** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
    // @ts-ignore
    const __VLS_237 = __VLS_asFunctionalComponent(__VLS_236, new __VLS_236({
        ...{ 'onClick': {} },
    }));
    const __VLS_238 = __VLS_237({
        ...{ 'onClick': {} },
    }, ...__VLS_functionalComponentArgsRest(__VLS_237));
    let __VLS_240;
    let __VLS_241;
    let __VLS_242;
    const __VLS_243 = {
        onClick: (...[$event]) => {
            __VLS_ctx.dialogVisible = false;
        }
    };
    __VLS_239.slots.default;
    var __VLS_239;
    const __VLS_244 = {}.ElButton;
    /** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
    // @ts-ignore
    const __VLS_245 = __VLS_asFunctionalComponent(__VLS_244, new __VLS_244({
        ...{ 'onClick': {} },
        type: "primary",
        loading: (__VLS_ctx.saving),
    }));
    const __VLS_246 = __VLS_245({
        ...{ 'onClick': {} },
        type: "primary",
        loading: (__VLS_ctx.saving),
    }, ...__VLS_functionalComponentArgsRest(__VLS_245));
    let __VLS_248;
    let __VLS_249;
    let __VLS_250;
    const __VLS_251 = {
        onClick: (__VLS_ctx.onSave)
    };
    __VLS_247.slots.default;
    var __VLS_247;
}
var __VLS_115;
const __VLS_252 = {}.ElDialog;
/** @type {[typeof __VLS_components.ElDialog, typeof __VLS_components.elDialog, typeof __VLS_components.ElDialog, typeof __VLS_components.elDialog, ]} */ ;
// @ts-ignore
const __VLS_253 = __VLS_asFunctionalComponent(__VLS_252, new __VLS_252({
    modelValue: (__VLS_ctx.trendVisible),
    title: "设备趋势（小时聚合）",
    width: "820px",
}));
const __VLS_254 = __VLS_253({
    modelValue: (__VLS_ctx.trendVisible),
    title: "设备趋势（小时聚合）",
    width: "820px",
}, ...__VLS_functionalComponentArgsRest(__VLS_253));
__VLS_255.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ref: "chartEl",
    ...{ style: {} },
});
/** @type {typeof __VLS_ctx.chartEl} */ ;
{
    const { footer: __VLS_thisSlot } = __VLS_255.slots;
    const __VLS_256 = {}.ElButton;
    /** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
    // @ts-ignore
    const __VLS_257 = __VLS_asFunctionalComponent(__VLS_256, new __VLS_256({
        ...{ 'onClick': {} },
    }));
    const __VLS_258 = __VLS_257({
        ...{ 'onClick': {} },
    }, ...__VLS_functionalComponentArgsRest(__VLS_257));
    let __VLS_260;
    let __VLS_261;
    let __VLS_262;
    const __VLS_263 = {
        onClick: (...[$event]) => {
            __VLS_ctx.trendVisible = false;
        }
    };
    __VLS_259.slots.default;
    var __VLS_259;
}
var __VLS_255;
/** @type {__VLS_StyleScopedClasses['sf-page']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-page-head']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-page-title']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-page-sub']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-page-actions']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-card']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-table-card']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-chip']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-muted']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-muted']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-muted']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-muted']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-muted']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-muted']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-chip']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-form-grid']} */ ;
var __VLS_dollars;
const __VLS_self = (await import('vue')).defineComponent({
    setup() {
        return {
            loading: loading,
            list: list,
            total: total,
            page: page,
            pageSize: pageSize,
            deviceTypeFilter: deviceTypeFilter,
            deviceTypeOptions: deviceTypeOptions,
            deviceTypeLabel: deviceTypeLabel,
            dialogVisible: dialogVisible,
            saving: saving,
            editing: editing,
            form: form,
            trendVisible: trendVisible,
            chartEl: chartEl,
            load: load,
            openCreate: openCreate,
            openEdit: openEdit,
            onSave: onSave,
            onDelete: onDelete,
            openTrend: openTrend,
        };
    },
});
export default (await import('vue')).defineComponent({
    setup() {
        return {};
    },
});
; /* PartiallyEnd: #4569/main.vue */
