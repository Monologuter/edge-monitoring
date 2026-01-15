import { computed, ref, watch } from "vue";
import { ElMessage } from "element-plus";
import { http } from "@/api/http";
const list = ref([]);
const loading = ref(false);
const page = ref(1);
const pageSize = ref(20);
const total = ref(0);
const status = ref(undefined);
const pendingCount = computed(() => list.value.filter((item) => item.status === "PENDING").length);
const sentCount = computed(() => list.value.filter((item) => item.status === "SENT").length);
const failedCount = computed(() => list.value.filter((item) => item.status === "FAILED").length);
function fmt(ts) {
    const d = new Date(ts);
    const pad = (n) => String(n).padStart(2, "0");
    return `${d.getFullYear()}-${pad(d.getMonth() + 1)}-${pad(d.getDate())} ${pad(d.getHours())}:${pad(d.getMinutes())}:${pad(d.getSeconds())}`;
}
async function load() {
    loading.value = true;
    try {
        const data = await http.get(`/api/v1/linkage/events?page=${page.value}&pageSize=${pageSize.value}${status.value ? `&status=${status.value}` : ""}`);
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
watch([page, pageSize, status], () => load());
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
    modelValue: (__VLS_ctx.status),
    placeholder: "状态",
    ...{ style: {} },
    clearable: true,
}));
const __VLS_2 = __VLS_1({
    modelValue: (__VLS_ctx.status),
    placeholder: "状态",
    ...{ style: {} },
    clearable: true,
}, ...__VLS_functionalComponentArgsRest(__VLS_1));
__VLS_3.slots.default;
const __VLS_4 = {}.ElOption;
/** @type {[typeof __VLS_components.ElOption, typeof __VLS_components.elOption, ]} */ ;
// @ts-ignore
const __VLS_5 = __VLS_asFunctionalComponent(__VLS_4, new __VLS_4({
    label: "PENDING",
    value: "PENDING",
}));
const __VLS_6 = __VLS_5({
    label: "PENDING",
    value: "PENDING",
}, ...__VLS_functionalComponentArgsRest(__VLS_5));
const __VLS_8 = {}.ElOption;
/** @type {[typeof __VLS_components.ElOption, typeof __VLS_components.elOption, ]} */ ;
// @ts-ignore
const __VLS_9 = __VLS_asFunctionalComponent(__VLS_8, new __VLS_8({
    label: "SENT",
    value: "SENT",
}));
const __VLS_10 = __VLS_9({
    label: "SENT",
    value: "SENT",
}, ...__VLS_functionalComponentArgsRest(__VLS_9));
const __VLS_12 = {}.ElOption;
/** @type {[typeof __VLS_components.ElOption, typeof __VLS_components.elOption, ]} */ ;
// @ts-ignore
const __VLS_13 = __VLS_asFunctionalComponent(__VLS_12, new __VLS_12({
    label: "FAILED",
    value: "FAILED",
}));
const __VLS_14 = __VLS_13({
    label: "FAILED",
    value: "FAILED",
}, ...__VLS_functionalComponentArgsRest(__VLS_13));
var __VLS_3;
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
});
(__VLS_ctx.pendingCount);
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
(__VLS_ctx.sentCount);
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
(__VLS_ctx.failedCount);
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
    prop: "id",
    label: "ID",
    width: "90",
}));
const __VLS_30 = __VLS_29({
    prop: "id",
    label: "ID",
    width: "90",
}, ...__VLS_functionalComponentArgsRest(__VLS_29));
const __VLS_32 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_33 = __VLS_asFunctionalComponent(__VLS_32, new __VLS_32({
    prop: "alarmId",
    label: "告警ID",
    width: "110",
}));
const __VLS_34 = __VLS_33({
    prop: "alarmId",
    label: "告警ID",
    width: "110",
}, ...__VLS_functionalComponentArgsRest(__VLS_33));
const __VLS_36 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_37 = __VLS_asFunctionalComponent(__VLS_36, new __VLS_36({
    prop: "linkageType",
    label: "联动类型",
    width: "140",
}));
const __VLS_38 = __VLS_37({
    prop: "linkageType",
    label: "联动类型",
    width: "140",
}, ...__VLS_functionalComponentArgsRest(__VLS_37));
const __VLS_40 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_41 = __VLS_asFunctionalComponent(__VLS_40, new __VLS_40({
    prop: "target",
    label: "目标",
    minWidth: "180",
}));
const __VLS_42 = __VLS_41({
    prop: "target",
    label: "目标",
    minWidth: "180",
}, ...__VLS_functionalComponentArgsRest(__VLS_41));
const __VLS_44 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_45 = __VLS_asFunctionalComponent(__VLS_44, new __VLS_44({
    prop: "payload",
    label: "内容",
    minWidth: "220",
    showOverflowTooltip: true,
}));
const __VLS_46 = __VLS_45({
    prop: "payload",
    label: "内容",
    minWidth: "220",
    showOverflowTooltip: true,
}, ...__VLS_functionalComponentArgsRest(__VLS_45));
const __VLS_48 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_49 = __VLS_asFunctionalComponent(__VLS_48, new __VLS_48({
    prop: "status",
    label: "状态",
    width: "110",
}));
const __VLS_50 = __VLS_49({
    prop: "status",
    label: "状态",
    width: "110",
}, ...__VLS_functionalComponentArgsRest(__VLS_49));
__VLS_51.slots.default;
{
    const { default: __VLS_thisSlot } = __VLS_51.slots;
    const [{ row }] = __VLS_getSlotParams(__VLS_thisSlot);
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({
        ...{ class: "sf-chip" },
        ...{ style: (row.status === 'FAILED'
                ? 'border-color: rgba(255,107,107,0.25)'
                : row.status === 'SENT'
                    ? 'border-color: rgba(39,183,167,0.25)'
                    : '') },
    });
    (row.status);
}
var __VLS_51;
const __VLS_52 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_53 = __VLS_asFunctionalComponent(__VLS_52, new __VLS_52({
    prop: "lastError",
    label: "错误信息",
    minWidth: "220",
    showOverflowTooltip: true,
}));
const __VLS_54 = __VLS_53({
    prop: "lastError",
    label: "错误信息",
    minWidth: "220",
    showOverflowTooltip: true,
}, ...__VLS_functionalComponentArgsRest(__VLS_53));
const __VLS_56 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_57 = __VLS_asFunctionalComponent(__VLS_56, new __VLS_56({
    prop: "createdAt",
    label: "创建时间",
    minWidth: "170",
}));
const __VLS_58 = __VLS_57({
    prop: "createdAt",
    label: "创建时间",
    minWidth: "170",
}, ...__VLS_functionalComponentArgsRest(__VLS_57));
__VLS_59.slots.default;
{
    const { default: __VLS_thisSlot } = __VLS_59.slots;
    const [{ row }] = __VLS_getSlotParams(__VLS_thisSlot);
    (row.createdAt ? __VLS_ctx.fmt(row.createdAt) : "—");
}
var __VLS_59;
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
    total: (__VLS_ctx.total),
    currentPage: (__VLS_ctx.page),
    pageSize: (__VLS_ctx.pageSize),
}));
const __VLS_62 = __VLS_61({
    ...{ 'onChange': {} },
    background: true,
    layout: "prev, pager, next, sizes, total",
    total: (__VLS_ctx.total),
    currentPage: (__VLS_ctx.page),
    pageSize: (__VLS_ctx.pageSize),
}, ...__VLS_functionalComponentArgsRest(__VLS_61));
let __VLS_64;
let __VLS_65;
let __VLS_66;
const __VLS_67 = {
    onChange: (__VLS_ctx.load)
};
var __VLS_63;
/** @type {__VLS_StyleScopedClasses['sf-page']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-page-head']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-page-title']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-page-sub']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-page-actions']} */ ;
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
/** @type {__VLS_StyleScopedClasses['sf-chip']} */ ;
var __VLS_dollars;
const __VLS_self = (await import('vue')).defineComponent({
    setup() {
        return {
            list: list,
            loading: loading,
            page: page,
            pageSize: pageSize,
            total: total,
            status: status,
            pendingCount: pendingCount,
            sentCount: sentCount,
            failedCount: failedCount,
            fmt: fmt,
            load: load,
        };
    },
});
export default (await import('vue')).defineComponent({
    setup() {
        return {};
    },
});
; /* PartiallyEnd: #4569/main.vue */
