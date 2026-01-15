import { computed, ref, watch } from "vue";
import { ElMessage } from "element-plus";
import { http } from "@/api/http";
const list = ref([]);
const loading = ref(false);
const page = ref(1);
const pageSize = ref(20);
const total = ref(0);
const onlineFilter = ref(null);
const onlineCount = computed(() => list.value.filter((item) => item.onlineStatus === 1).length);
const offlineCount = computed(() => list.value.filter((item) => item.onlineStatus !== 1).length);
async function load() {
    loading.value = true;
    try {
        const data = await http.get(`/api/v1/devices?page=${page.value}&pageSize=${pageSize.value}&deviceType=3`);
        const incoming = data.list;
        list.value = onlineFilter.value === null ? incoming : incoming.filter((item) => item.onlineStatus === onlineFilter.value);
        total.value = data.total;
    }
    catch (e) {
        ElMessage.error(e?.message || "加载失败");
    }
    finally {
        loading.value = false;
    }
}
watch([page, pageSize], () => load());
watch(onlineFilter, () => {
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
    modelValue: (__VLS_ctx.onlineFilter),
    placeholder: "在线状态",
    ...{ style: {} },
    clearable: true,
}));
const __VLS_2 = __VLS_1({
    modelValue: (__VLS_ctx.onlineFilter),
    placeholder: "在线状态",
    ...{ style: {} },
    clearable: true,
}, ...__VLS_functionalComponentArgsRest(__VLS_1));
__VLS_3.slots.default;
const __VLS_4 = {}.ElOption;
/** @type {[typeof __VLS_components.ElOption, typeof __VLS_components.elOption, ]} */ ;
// @ts-ignore
const __VLS_5 = __VLS_asFunctionalComponent(__VLS_4, new __VLS_4({
    label: "在线",
    value: (1),
}));
const __VLS_6 = __VLS_5({
    label: "在线",
    value: (1),
}, ...__VLS_functionalComponentArgsRest(__VLS_5));
const __VLS_8 = {}.ElOption;
/** @type {[typeof __VLS_components.ElOption, typeof __VLS_components.elOption, ]} */ ;
// @ts-ignore
const __VLS_9 = __VLS_asFunctionalComponent(__VLS_8, new __VLS_8({
    label: "离线",
    value: (0),
}));
const __VLS_10 = __VLS_9({
    label: "离线",
    value: (0),
}, ...__VLS_functionalComponentArgsRest(__VLS_9));
var __VLS_3;
const __VLS_12 = {}.ElButton;
/** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
// @ts-ignore
const __VLS_13 = __VLS_asFunctionalComponent(__VLS_12, new __VLS_12({
    ...{ 'onClick': {} },
}));
const __VLS_14 = __VLS_13({
    ...{ 'onClick': {} },
}, ...__VLS_functionalComponentArgsRest(__VLS_13));
let __VLS_16;
let __VLS_17;
let __VLS_18;
const __VLS_19 = {
    onClick: (__VLS_ctx.load)
};
__VLS_15.slots.default;
var __VLS_15;
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
(__VLS_ctx.onlineCount);
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
(__VLS_ctx.offlineCount);
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
(__VLS_ctx.total);
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-card sf-table-card" },
});
const __VLS_20 = {}.ElTable;
/** @type {[typeof __VLS_components.ElTable, typeof __VLS_components.elTable, typeof __VLS_components.ElTable, typeof __VLS_components.elTable, ]} */ ;
// @ts-ignore
const __VLS_21 = __VLS_asFunctionalComponent(__VLS_20, new __VLS_20({
    data: (__VLS_ctx.list),
    ...{ style: {} },
}));
const __VLS_22 = __VLS_21({
    data: (__VLS_ctx.list),
    ...{ style: {} },
}, ...__VLS_functionalComponentArgsRest(__VLS_21));
__VLS_asFunctionalDirective(__VLS_directives.vLoading)(null, { ...__VLS_directiveBindingRestFields, value: (__VLS_ctx.loading) }, null, null);
__VLS_23.slots.default;
const __VLS_24 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_25 = __VLS_asFunctionalComponent(__VLS_24, new __VLS_24({
    prop: "deviceCode",
    label: "设备编码",
    minWidth: "140",
}));
const __VLS_26 = __VLS_25({
    prop: "deviceCode",
    label: "设备编码",
    minWidth: "140",
}, ...__VLS_functionalComponentArgsRest(__VLS_25));
const __VLS_28 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_29 = __VLS_asFunctionalComponent(__VLS_28, new __VLS_28({
    prop: "deviceName",
    label: "设备名称",
    minWidth: "180",
}));
const __VLS_30 = __VLS_29({
    prop: "deviceName",
    label: "设备名称",
    minWidth: "180",
}, ...__VLS_functionalComponentArgsRest(__VLS_29));
const __VLS_32 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_33 = __VLS_asFunctionalComponent(__VLS_32, new __VLS_32({
    prop: "locationName",
    label: "位置",
    minWidth: "160",
}));
const __VLS_34 = __VLS_33({
    prop: "locationName",
    label: "位置",
    minWidth: "160",
}, ...__VLS_functionalComponentArgsRest(__VLS_33));
const __VLS_36 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_37 = __VLS_asFunctionalComponent(__VLS_36, new __VLS_36({
    prop: "storeNum",
    label: "仓库编码",
    minWidth: "140",
}));
const __VLS_38 = __VLS_37({
    prop: "storeNum",
    label: "仓库编码",
    minWidth: "140",
}, ...__VLS_functionalComponentArgsRest(__VLS_37));
const __VLS_40 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_41 = __VLS_asFunctionalComponent(__VLS_40, new __VLS_40({
    prop: "storeroomNum",
    label: "库房编码",
    minWidth: "140",
}));
const __VLS_42 = __VLS_41({
    prop: "storeroomNum",
    label: "库房编码",
    minWidth: "140",
}, ...__VLS_functionalComponentArgsRest(__VLS_41));
const __VLS_44 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_45 = __VLS_asFunctionalComponent(__VLS_44, new __VLS_44({
    label: "阈值",
    minWidth: "160",
}));
const __VLS_46 = __VLS_45({
    label: "阈值",
    minWidth: "160",
}, ...__VLS_functionalComponentArgsRest(__VLS_45));
__VLS_47.slots.default;
{
    const { default: __VLS_thisSlot } = __VLS_47.slots;
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
var __VLS_47;
const __VLS_48 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_49 = __VLS_asFunctionalComponent(__VLS_48, new __VLS_48({
    prop: "onlineStatus",
    label: "在线",
    width: "90",
}));
const __VLS_50 = __VLS_49({
    prop: "onlineStatus",
    label: "在线",
    width: "90",
}, ...__VLS_functionalComponentArgsRest(__VLS_49));
__VLS_51.slots.default;
{
    const { default: __VLS_thisSlot } = __VLS_51.slots;
    const [{ row }] = __VLS_getSlotParams(__VLS_thisSlot);
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({
        ...{ class: "sf-chip" },
        ...{ style: (row.onlineStatus === 1 ? 'border-color: rgba(51,209,122,0.25)' : '') },
    });
    (row.onlineStatus === 1 ? "在线" : "离线");
}
var __VLS_51;
var __VLS_23;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ style: {} },
});
const __VLS_52 = {}.ElPagination;
/** @type {[typeof __VLS_components.ElPagination, typeof __VLS_components.elPagination, ]} */ ;
// @ts-ignore
const __VLS_53 = __VLS_asFunctionalComponent(__VLS_52, new __VLS_52({
    ...{ 'onChange': {} },
    background: true,
    layout: "prev, pager, next, sizes, total",
    total: (__VLS_ctx.total),
    currentPage: (__VLS_ctx.page),
    pageSize: (__VLS_ctx.pageSize),
}));
const __VLS_54 = __VLS_53({
    ...{ 'onChange': {} },
    background: true,
    layout: "prev, pager, next, sizes, total",
    total: (__VLS_ctx.total),
    currentPage: (__VLS_ctx.page),
    pageSize: (__VLS_ctx.pageSize),
}, ...__VLS_functionalComponentArgsRest(__VLS_53));
let __VLS_56;
let __VLS_57;
let __VLS_58;
const __VLS_59 = {
    onChange: (__VLS_ctx.load)
};
var __VLS_55;
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
/** @type {__VLS_StyleScopedClasses['sf-muted']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-muted']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-muted']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-muted']} */ ;
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
            onlineFilter: onlineFilter,
            onlineCount: onlineCount,
            offlineCount: offlineCount,
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
