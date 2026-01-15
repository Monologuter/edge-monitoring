import { ref, onMounted } from "vue";
import { ElMessage } from "element-plus";
import { http } from "@/api/http";
const list = ref([]);
const loading = ref(false);
const dialogVisible = ref(false);
const editing = ref(null);
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
        const data = await http.get("/api/v1/device-api-keys");
        list.value = data || [];
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
    Object.assign(form.value, {
        apiKey: "",
        apiSecret: "",
        enabled: 1,
        allowedIps: "",
        rateLimitPerMinute: 60
    });
    dialogVisible.value = true;
}
function openEdit(row) {
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
        }
        else {
            if (!form.value.apiSecret) {
                ElMessage.warning("请输入API Secret");
                return;
            }
            await http.post("/api/v1/device-api-keys", form.value);
            ElMessage.success("创建成功");
        }
        dialogVisible.value = false;
        await load();
    }
    catch (e) {
        ElMessage.error(e?.message || "操作失败");
    }
}
async function handleDelete(id) {
    try {
        await http.del(`/api/v1/device-api-keys/${id}`);
        ElMessage.success("删除成功");
        await load();
    }
    catch (e) {
        ElMessage.error(e?.message || "删除失败");
    }
}
onMounted(() => {
    load();
});
debugger; /* PartiallyEnd: #3632/scriptSetup.vue */
const __VLS_ctx = {};
let __VLS_components;
let __VLS_directives;
/** @type {__VLS_StyleScopedClasses['sf-header']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-muted']} */ ;
// CSS variable injection 
// CSS variable injection end 
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-page sf-device-api-keys" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-header" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.h1, __VLS_intrinsicElements.h1)({});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-actions" },
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
    onClick: (__VLS_ctx.openCreate)
};
__VLS_3.slots.default;
var __VLS_3;
const __VLS_8 = {}.ElTable;
/** @type {[typeof __VLS_components.ElTable, typeof __VLS_components.elTable, typeof __VLS_components.ElTable, typeof __VLS_components.elTable, ]} */ ;
// @ts-ignore
const __VLS_9 = __VLS_asFunctionalComponent(__VLS_8, new __VLS_8({
    data: (__VLS_ctx.list),
    stripe: true,
}));
const __VLS_10 = __VLS_9({
    data: (__VLS_ctx.list),
    stripe: true,
}, ...__VLS_functionalComponentArgsRest(__VLS_9));
__VLS_asFunctionalDirective(__VLS_directives.vLoading)(null, { ...__VLS_directiveBindingRestFields, value: (__VLS_ctx.loading) }, null, null);
__VLS_11.slots.default;
const __VLS_12 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_13 = __VLS_asFunctionalComponent(__VLS_12, new __VLS_12({
    prop: "apiKey",
    label: "API Key",
    width: "200",
}));
const __VLS_14 = __VLS_13({
    prop: "apiKey",
    label: "API Key",
    width: "200",
}, ...__VLS_functionalComponentArgsRest(__VLS_13));
const __VLS_16 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_17 = __VLS_asFunctionalComponent(__VLS_16, new __VLS_16({
    prop: "apiSecret",
    label: "API Secret",
    width: "150",
}));
const __VLS_18 = __VLS_17({
    prop: "apiSecret",
    label: "API Secret",
    width: "150",
}, ...__VLS_functionalComponentArgsRest(__VLS_17));
__VLS_19.slots.default;
{
    const { default: __VLS_thisSlot } = __VLS_19.slots;
    const [{ row }] = __VLS_getSlotParams(__VLS_thisSlot);
    __VLS_asFunctionalElement(__VLS_intrinsicElements.code, __VLS_intrinsicElements.code)({
        ...{ class: "sf-muted" },
    });
    (row.apiSecret);
}
var __VLS_19;
const __VLS_20 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_21 = __VLS_asFunctionalComponent(__VLS_20, new __VLS_20({
    label: "状态",
    width: "80",
}));
const __VLS_22 = __VLS_21({
    label: "状态",
    width: "80",
}, ...__VLS_functionalComponentArgsRest(__VLS_21));
__VLS_23.slots.default;
{
    const { default: __VLS_thisSlot } = __VLS_23.slots;
    const [{ row }] = __VLS_getSlotParams(__VLS_thisSlot);
    const __VLS_24 = {}.ElTag;
    /** @type {[typeof __VLS_components.ElTag, typeof __VLS_components.elTag, typeof __VLS_components.ElTag, typeof __VLS_components.elTag, ]} */ ;
    // @ts-ignore
    const __VLS_25 = __VLS_asFunctionalComponent(__VLS_24, new __VLS_24({
        type: (row.enabled ? 'success' : 'danger'),
    }));
    const __VLS_26 = __VLS_25({
        type: (row.enabled ? 'success' : 'danger'),
    }, ...__VLS_functionalComponentArgsRest(__VLS_25));
    __VLS_27.slots.default;
    (row.enabled ? "启用" : "禁用");
    var __VLS_27;
}
var __VLS_23;
const __VLS_28 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_29 = __VLS_asFunctionalComponent(__VLS_28, new __VLS_28({
    prop: "allowedIps",
    label: "允许IP",
    width: "200",
}));
const __VLS_30 = __VLS_29({
    prop: "allowedIps",
    label: "允许IP",
    width: "200",
}, ...__VLS_functionalComponentArgsRest(__VLS_29));
__VLS_31.slots.default;
{
    const { default: __VLS_thisSlot } = __VLS_31.slots;
    const [{ row }] = __VLS_getSlotParams(__VLS_thisSlot);
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({
        ...{ class: "sf-muted" },
    });
    (row.allowedIps || "不限");
}
var __VLS_31;
const __VLS_32 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_33 = __VLS_asFunctionalComponent(__VLS_32, new __VLS_32({
    prop: "rateLimitPerMinute",
    label: "限流(次/分)",
    width: "120",
}));
const __VLS_34 = __VLS_33({
    prop: "rateLimitPerMinute",
    label: "限流(次/分)",
    width: "120",
}, ...__VLS_functionalComponentArgsRest(__VLS_33));
const __VLS_36 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_37 = __VLS_asFunctionalComponent(__VLS_36, new __VLS_36({
    label: "操作",
    width: "180",
    fixed: "right",
}));
const __VLS_38 = __VLS_37({
    label: "操作",
    width: "180",
    fixed: "right",
}, ...__VLS_functionalComponentArgsRest(__VLS_37));
__VLS_39.slots.default;
{
    const { default: __VLS_thisSlot } = __VLS_39.slots;
    const [{ row }] = __VLS_getSlotParams(__VLS_thisSlot);
    const __VLS_40 = {}.ElButton;
    /** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
    // @ts-ignore
    const __VLS_41 = __VLS_asFunctionalComponent(__VLS_40, new __VLS_40({
        ...{ 'onClick': {} },
        link: true,
        type: "primary",
        size: "small",
    }));
    const __VLS_42 = __VLS_41({
        ...{ 'onClick': {} },
        link: true,
        type: "primary",
        size: "small",
    }, ...__VLS_functionalComponentArgsRest(__VLS_41));
    let __VLS_44;
    let __VLS_45;
    let __VLS_46;
    const __VLS_47 = {
        onClick: (...[$event]) => {
            __VLS_ctx.openEdit(row);
        }
    };
    __VLS_43.slots.default;
    var __VLS_43;
    const __VLS_48 = {}.ElPopconfirm;
    /** @type {[typeof __VLS_components.ElPopconfirm, typeof __VLS_components.elPopconfirm, typeof __VLS_components.ElPopconfirm, typeof __VLS_components.elPopconfirm, ]} */ ;
    // @ts-ignore
    const __VLS_49 = __VLS_asFunctionalComponent(__VLS_48, new __VLS_48({
        ...{ 'onConfirm': {} },
        title: "确定删除此密钥?",
    }));
    const __VLS_50 = __VLS_49({
        ...{ 'onConfirm': {} },
        title: "确定删除此密钥?",
    }, ...__VLS_functionalComponentArgsRest(__VLS_49));
    let __VLS_52;
    let __VLS_53;
    let __VLS_54;
    const __VLS_55 = {
        onConfirm: (...[$event]) => {
            __VLS_ctx.handleDelete(row.id);
        }
    };
    __VLS_51.slots.default;
    {
        const { reference: __VLS_thisSlot } = __VLS_51.slots;
        const __VLS_56 = {}.ElButton;
        /** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
        // @ts-ignore
        const __VLS_57 = __VLS_asFunctionalComponent(__VLS_56, new __VLS_56({
            link: true,
            type: "danger",
            size: "small",
        }));
        const __VLS_58 = __VLS_57({
            link: true,
            type: "danger",
            size: "small",
        }, ...__VLS_functionalComponentArgsRest(__VLS_57));
        __VLS_59.slots.default;
        var __VLS_59;
    }
    var __VLS_51;
}
var __VLS_39;
var __VLS_11;
const __VLS_60 = {}.ElDialog;
/** @type {[typeof __VLS_components.ElDialog, typeof __VLS_components.elDialog, typeof __VLS_components.ElDialog, typeof __VLS_components.elDialog, ]} */ ;
// @ts-ignore
const __VLS_61 = __VLS_asFunctionalComponent(__VLS_60, new __VLS_60({
    modelValue: (__VLS_ctx.dialogVisible),
    title: (__VLS_ctx.editing ? '编辑密钥' : '新增密钥'),
    width: "500px",
}));
const __VLS_62 = __VLS_61({
    modelValue: (__VLS_ctx.dialogVisible),
    title: (__VLS_ctx.editing ? '编辑密钥' : '新增密钥'),
    width: "500px",
}, ...__VLS_functionalComponentArgsRest(__VLS_61));
__VLS_63.slots.default;
const __VLS_64 = {}.ElForm;
/** @type {[typeof __VLS_components.ElForm, typeof __VLS_components.elForm, typeof __VLS_components.ElForm, typeof __VLS_components.elForm, ]} */ ;
// @ts-ignore
const __VLS_65 = __VLS_asFunctionalComponent(__VLS_64, new __VLS_64({
    model: (__VLS_ctx.form),
    labelWidth: "100px",
}));
const __VLS_66 = __VLS_65({
    model: (__VLS_ctx.form),
    labelWidth: "100px",
}, ...__VLS_functionalComponentArgsRest(__VLS_65));
__VLS_67.slots.default;
const __VLS_68 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_69 = __VLS_asFunctionalComponent(__VLS_68, new __VLS_68({
    label: "API Key",
}));
const __VLS_70 = __VLS_69({
    label: "API Key",
}, ...__VLS_functionalComponentArgsRest(__VLS_69));
__VLS_71.slots.default;
const __VLS_72 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_73 = __VLS_asFunctionalComponent(__VLS_72, new __VLS_72({
    modelValue: (__VLS_ctx.form.apiKey),
    disabled: (__VLS_ctx.editing),
    placeholder: "请输入API Key",
}));
const __VLS_74 = __VLS_73({
    modelValue: (__VLS_ctx.form.apiKey),
    disabled: (__VLS_ctx.editing),
    placeholder: "请输入API Key",
}, ...__VLS_functionalComponentArgsRest(__VLS_73));
var __VLS_71;
const __VLS_76 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_77 = __VLS_asFunctionalComponent(__VLS_76, new __VLS_76({
    label: "API Secret",
}));
const __VLS_78 = __VLS_77({
    label: "API Secret",
}, ...__VLS_functionalComponentArgsRest(__VLS_77));
__VLS_79.slots.default;
const __VLS_80 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_81 = __VLS_asFunctionalComponent(__VLS_80, new __VLS_80({
    modelValue: (__VLS_ctx.form.apiSecret),
    type: "password",
    showPassword: true,
    placeholder: "请输入API Secret",
}));
const __VLS_82 = __VLS_81({
    modelValue: (__VLS_ctx.form.apiSecret),
    type: "password",
    showPassword: true,
    placeholder: "请输入API Secret",
}, ...__VLS_functionalComponentArgsRest(__VLS_81));
var __VLS_79;
const __VLS_84 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_85 = __VLS_asFunctionalComponent(__VLS_84, new __VLS_84({
    label: "状态",
}));
const __VLS_86 = __VLS_85({
    label: "状态",
}, ...__VLS_functionalComponentArgsRest(__VLS_85));
__VLS_87.slots.default;
const __VLS_88 = {}.ElSwitch;
/** @type {[typeof __VLS_components.ElSwitch, typeof __VLS_components.elSwitch, ]} */ ;
// @ts-ignore
const __VLS_89 = __VLS_asFunctionalComponent(__VLS_88, new __VLS_88({
    modelValue: (__VLS_ctx.form.enabled),
    activeValue: (1),
    inactiveValue: (0),
}));
const __VLS_90 = __VLS_89({
    modelValue: (__VLS_ctx.form.enabled),
    activeValue: (1),
    inactiveValue: (0),
}, ...__VLS_functionalComponentArgsRest(__VLS_89));
var __VLS_87;
const __VLS_92 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_93 = __VLS_asFunctionalComponent(__VLS_92, new __VLS_92({
    label: "允许IP",
}));
const __VLS_94 = __VLS_93({
    label: "允许IP",
}, ...__VLS_functionalComponentArgsRest(__VLS_93));
__VLS_95.slots.default;
const __VLS_96 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_97 = __VLS_asFunctionalComponent(__VLS_96, new __VLS_96({
    modelValue: (__VLS_ctx.form.allowedIps),
    placeholder: "例如: 192.168.1.0/24,10.0.0.1",
}));
const __VLS_98 = __VLS_97({
    modelValue: (__VLS_ctx.form.allowedIps),
    placeholder: "例如: 192.168.1.0/24,10.0.0.1",
}, ...__VLS_functionalComponentArgsRest(__VLS_97));
var __VLS_95;
const __VLS_100 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_101 = __VLS_asFunctionalComponent(__VLS_100, new __VLS_100({
    label: "限流(次/分)",
}));
const __VLS_102 = __VLS_101({
    label: "限流(次/分)",
}, ...__VLS_functionalComponentArgsRest(__VLS_101));
__VLS_103.slots.default;
const __VLS_104 = {}.ElInputNumber;
/** @type {[typeof __VLS_components.ElInputNumber, typeof __VLS_components.elInputNumber, ]} */ ;
// @ts-ignore
const __VLS_105 = __VLS_asFunctionalComponent(__VLS_104, new __VLS_104({
    modelValue: (__VLS_ctx.form.rateLimitPerMinute),
    min: (1),
    max: (1000),
}));
const __VLS_106 = __VLS_105({
    modelValue: (__VLS_ctx.form.rateLimitPerMinute),
    min: (1),
    max: (1000),
}, ...__VLS_functionalComponentArgsRest(__VLS_105));
var __VLS_103;
var __VLS_67;
{
    const { footer: __VLS_thisSlot } = __VLS_63.slots;
    const __VLS_108 = {}.ElButton;
    /** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
    // @ts-ignore
    const __VLS_109 = __VLS_asFunctionalComponent(__VLS_108, new __VLS_108({
        ...{ 'onClick': {} },
    }));
    const __VLS_110 = __VLS_109({
        ...{ 'onClick': {} },
    }, ...__VLS_functionalComponentArgsRest(__VLS_109));
    let __VLS_112;
    let __VLS_113;
    let __VLS_114;
    const __VLS_115 = {
        onClick: (...[$event]) => {
            __VLS_ctx.dialogVisible = false;
        }
    };
    __VLS_111.slots.default;
    var __VLS_111;
    const __VLS_116 = {}.ElButton;
    /** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
    // @ts-ignore
    const __VLS_117 = __VLS_asFunctionalComponent(__VLS_116, new __VLS_116({
        ...{ 'onClick': {} },
        type: "primary",
    }));
    const __VLS_118 = __VLS_117({
        ...{ 'onClick': {} },
        type: "primary",
    }, ...__VLS_functionalComponentArgsRest(__VLS_117));
    let __VLS_120;
    let __VLS_121;
    let __VLS_122;
    const __VLS_123 = {
        onClick: (__VLS_ctx.submit)
    };
    __VLS_119.slots.default;
    var __VLS_119;
}
var __VLS_63;
/** @type {__VLS_StyleScopedClasses['sf-page']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-device-api-keys']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-header']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-actions']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-muted']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-muted']} */ ;
var __VLS_dollars;
const __VLS_self = (await import('vue')).defineComponent({
    setup() {
        return {
            list: list,
            loading: loading,
            dialogVisible: dialogVisible,
            editing: editing,
            form: form,
            openCreate: openCreate,
            openEdit: openEdit,
            submit: submit,
            handleDelete: handleDelete,
        };
    },
});
export default (await import('vue')).defineComponent({
    setup() {
        return {};
    },
});
; /* PartiallyEnd: #4569/main.vue */
