import { reactive, ref, watch } from "vue";
import { ElMessage, ElMessageBox } from "element-plus";
import { fileUrl, http, rawHttp, uploadFile } from "@/api/http";
const keyword = ref("");
const page = ref(1);
const pageSize = ref(20);
const total = ref(0);
const loading = ref(false);
const list = ref([]);
const dialogVisible = ref(false);
const saving = ref(false);
const editing = ref(null);
const form = reactive({
    companyCode: "",
    companyName: "",
    creditCode: "",
    principalName: "",
    companyStatus: "",
    businessLicense: "",
    businessLicenseFileId: null,
    businessLicenseStart: "",
    businessLicenseEnd: "",
    businessLicenseScope: "",
    businessLicenseIssuingAuthority: "",
    address: "",
    registerAddress: "",
    storageAddress: "",
    dosage: null,
    reservoirArea: null,
    storeroomArea: null,
    longitude: null,
    latitude: null
});
async function load() {
    loading.value = true;
    try {
        const data = await http.get(`/api/v1/companies?page=${page.value}&pageSize=${pageSize.value}${keyword.value ? `&keyword=${encodeURIComponent(keyword.value)}` : ""}`);
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
        companyName: "",
        creditCode: "",
        principalName: "",
        companyStatus: "",
        businessLicense: "",
        businessLicenseFileId: null,
        businessLicenseStart: null,
        businessLicenseEnd: null,
        businessLicenseScope: "",
        businessLicenseIssuingAuthority: "",
        address: "",
        registerAddress: "",
        storageAddress: "",
        dosage: null,
        reservoirArea: null,
        storeroomArea: null,
        longitude: null,
        latitude: null
    });
    dialogVisible.value = true;
}
function openEdit(row) {
    editing.value = row;
    Object.assign(form, {
        companyCode: row.companyCode,
        companyName: row.companyName,
        creditCode: row.creditCode || "",
        principalName: row.principalName || "",
        companyStatus: row.companyStatus || "",
        businessLicense: row.businessLicense || "",
        businessLicenseFileId: row.businessLicenseFileId || null,
        businessLicenseStart: row.businessLicenseStart || null,
        businessLicenseEnd: row.businessLicenseEnd || null,
        businessLicenseScope: row.businessLicenseScope || "",
        businessLicenseIssuingAuthority: row.businessLicenseIssuingAuthority || "",
        address: row.address || "",
        registerAddress: row.registerAddress || "",
        storageAddress: row.storageAddress || "",
        dosage: row.dosage ?? null,
        reservoirArea: row.reservoirArea ?? null,
        storeroomArea: row.storeroomArea ?? null,
        longitude: row.longitude ?? null,
        latitude: row.latitude ?? null
    });
    dialogVisible.value = true;
}
function isJpg(file) {
    const name = file.name.toLowerCase();
    return file.type === "image/jpeg" || name.endsWith(".jpg") || name.endsWith(".jpeg");
}
async function beforeUpload(file) {
    if (!isJpg(file)) {
        ElMessage.warning("请上传 JPG 图片");
        return false;
    }
    try {
        const out = await uploadFile("license", file);
        form.businessLicenseFileId = out.id;
        ElMessage.success("上传成功");
    }
    catch (e) {
        ElMessage.error(e?.message || "上传失败");
    }
    return false;
}
async function onSave() {
    if (!form.companyCode || !form.companyName) {
        ElMessage.warning("请填写企业编码与企业名称");
        return;
    }
    saving.value = true;
    try {
        if (!editing.value?.id) {
            await http.post("/api/v1/companies", { ...form });
            ElMessage.success("新增成功");
        }
        else {
            await http.put("/api/v1/companies", { id: editing.value.id, ...form });
            ElMessage.success("保存成功");
        }
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
    const ok = await ElMessageBox.confirm("确认删除该企业？", "提示", { type: "warning" }).catch(() => null);
    if (!ok)
        return;
    try {
        await http.del(`/api/v1/companies/${id}`);
        ElMessage.success("删除成功");
        await load();
    }
    catch (e) {
        ElMessage.error(e?.message || "删除失败");
    }
}
async function importCompanies(file) {
    const fd = new FormData();
    fd.append("file", file);
    try {
        const res = await rawHttp.post("/api/v1/companies/import", fd, {
            headers: { "Content-Type": "multipart/form-data" }
        });
        ElMessage.success(`导入完成：成功 ${res.data.successCount} 条，失败 ${res.data.failCount} 条`);
        await load();
    }
    catch (e) {
        ElMessage.error(e?.message || "导入失败");
    }
    return false;
}
watch([page, pageSize], () => load());
watch(keyword, () => {
    page.value = 1;
    load();
});
load();
debugger; /* PartiallyEnd: #3632/scriptSetup.vue */
const __VLS_ctx = {};
let __VLS_components;
let __VLS_directives;
/** @type {__VLS_StyleScopedClasses['detail-item']} */ ;
/** @type {__VLS_StyleScopedClasses['company-detail']} */ ;
/** @type {__VLS_StyleScopedClasses['company-detail']} */ ;
// CSS variable injection 
// CSS variable injection end 
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
const __VLS_0 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_1 = __VLS_asFunctionalComponent(__VLS_0, new __VLS_0({
    modelValue: (__VLS_ctx.keyword),
    placeholder: "企业编码/名称",
    ...{ style: {} },
    clearable: true,
}));
const __VLS_2 = __VLS_1({
    modelValue: (__VLS_ctx.keyword),
    placeholder: "企业编码/名称",
    ...{ style: {} },
    clearable: true,
}, ...__VLS_functionalComponentArgsRest(__VLS_1));
const __VLS_4 = {}.ElButton;
/** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
// @ts-ignore
const __VLS_5 = __VLS_asFunctionalComponent(__VLS_4, new __VLS_4({
    ...{ 'onClick': {} },
    type: "primary",
}));
const __VLS_6 = __VLS_5({
    ...{ 'onClick': {} },
    type: "primary",
}, ...__VLS_functionalComponentArgsRest(__VLS_5));
let __VLS_8;
let __VLS_9;
let __VLS_10;
const __VLS_11 = {
    onClick: (__VLS_ctx.openCreate)
};
__VLS_7.slots.default;
var __VLS_7;
const __VLS_12 = {}.ElUpload;
/** @type {[typeof __VLS_components.ElUpload, typeof __VLS_components.elUpload, typeof __VLS_components.ElUpload, typeof __VLS_components.elUpload, ]} */ ;
// @ts-ignore
const __VLS_13 = __VLS_asFunctionalComponent(__VLS_12, new __VLS_12({
    showFileList: (false),
    beforeUpload: (__VLS_ctx.importCompanies),
    accept: ".csv",
}));
const __VLS_14 = __VLS_13({
    showFileList: (false),
    beforeUpload: (__VLS_ctx.importCompanies),
    accept: ".csv",
}, ...__VLS_functionalComponentArgsRest(__VLS_13));
__VLS_15.slots.default;
const __VLS_16 = {}.ElButton;
/** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
// @ts-ignore
const __VLS_17 = __VLS_asFunctionalComponent(__VLS_16, new __VLS_16({}));
const __VLS_18 = __VLS_17({}, ...__VLS_functionalComponentArgsRest(__VLS_17));
__VLS_19.slots.default;
var __VLS_19;
var __VLS_15;
const __VLS_20 = {}.ElButton;
/** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
// @ts-ignore
const __VLS_21 = __VLS_asFunctionalComponent(__VLS_20, new __VLS_20({
    ...{ 'onClick': {} },
}));
const __VLS_22 = __VLS_21({
    ...{ 'onClick': {} },
}, ...__VLS_functionalComponentArgsRest(__VLS_21));
let __VLS_24;
let __VLS_25;
let __VLS_26;
const __VLS_27 = {
    onClick: (__VLS_ctx.load)
};
__VLS_23.slots.default;
var __VLS_23;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-card sf-table-card" },
});
const __VLS_28 = {}.ElTable;
/** @type {[typeof __VLS_components.ElTable, typeof __VLS_components.elTable, typeof __VLS_components.ElTable, typeof __VLS_components.elTable, ]} */ ;
// @ts-ignore
const __VLS_29 = __VLS_asFunctionalComponent(__VLS_28, new __VLS_28({
    data: (__VLS_ctx.list),
    ...{ style: {} },
}));
const __VLS_30 = __VLS_29({
    data: (__VLS_ctx.list),
    ...{ style: {} },
}, ...__VLS_functionalComponentArgsRest(__VLS_29));
__VLS_asFunctionalDirective(__VLS_directives.vLoading)(null, { ...__VLS_directiveBindingRestFields, value: (__VLS_ctx.loading) }, null, null);
__VLS_31.slots.default;
const __VLS_32 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_33 = __VLS_asFunctionalComponent(__VLS_32, new __VLS_32({
    type: "expand",
    width: "44",
}));
const __VLS_34 = __VLS_33({
    type: "expand",
    width: "44",
}, ...__VLS_functionalComponentArgsRest(__VLS_33));
__VLS_35.slots.default;
{
    const { default: __VLS_thisSlot } = __VLS_35.slots;
    const [{ row }] = __VLS_getSlotParams(__VLS_thisSlot);
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
        ...{ class: "company-detail" },
    });
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
        ...{ class: "detail-item" },
    });
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({
        ...{ class: "label" },
    });
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
    (row.registerAddress || "—");
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
        ...{ class: "detail-item" },
    });
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({
        ...{ class: "label" },
    });
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
    (row.storageAddress || "—");
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
        ...{ class: "detail-item" },
    });
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({
        ...{ class: "label" },
    });
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
    (row.address || "—");
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
        ...{ class: "detail-item" },
    });
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({
        ...{ class: "label" },
    });
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
    (row.storeroomArea ?? "—");
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
        ...{ class: "detail-item" },
    });
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({
        ...{ class: "label" },
    });
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
    (row.reservoirArea ?? "—");
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
        ...{ class: "detail-item" },
    });
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({
        ...{ class: "label" },
    });
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
    (row.dosage ?? "—");
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
        ...{ class: "detail-item" },
    });
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({
        ...{ class: "label" },
    });
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
    (row.longitude ?? "—");
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
        ...{ class: "detail-item" },
    });
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({
        ...{ class: "label" },
    });
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
    (row.latitude ?? "—");
}
var __VLS_35;
const __VLS_36 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_37 = __VLS_asFunctionalComponent(__VLS_36, new __VLS_36({
    prop: "companyCode",
    label: "企业编码",
    minWidth: "140",
}));
const __VLS_38 = __VLS_37({
    prop: "companyCode",
    label: "企业编码",
    minWidth: "140",
}, ...__VLS_functionalComponentArgsRest(__VLS_37));
const __VLS_40 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_41 = __VLS_asFunctionalComponent(__VLS_40, new __VLS_40({
    prop: "companyName",
    label: "企业名称",
    minWidth: "220",
}));
const __VLS_42 = __VLS_41({
    prop: "companyName",
    label: "企业名称",
    minWidth: "220",
}, ...__VLS_functionalComponentArgsRest(__VLS_41));
const __VLS_44 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_45 = __VLS_asFunctionalComponent(__VLS_44, new __VLS_44({
    prop: "creditCode",
    label: "信用代码",
    minWidth: "180",
}));
const __VLS_46 = __VLS_45({
    prop: "creditCode",
    label: "信用代码",
    minWidth: "180",
}, ...__VLS_functionalComponentArgsRest(__VLS_45));
const __VLS_48 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_49 = __VLS_asFunctionalComponent(__VLS_48, new __VLS_48({
    prop: "principalName",
    label: "主要负责人",
    width: "140",
}));
const __VLS_50 = __VLS_49({
    prop: "principalName",
    label: "主要负责人",
    width: "140",
}, ...__VLS_functionalComponentArgsRest(__VLS_49));
const __VLS_52 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_53 = __VLS_asFunctionalComponent(__VLS_52, new __VLS_52({
    prop: "companyStatus",
    label: "状态",
    width: "120",
}));
const __VLS_54 = __VLS_53({
    prop: "companyStatus",
    label: "状态",
    width: "120",
}, ...__VLS_functionalComponentArgsRest(__VLS_53));
const __VLS_56 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_57 = __VLS_asFunctionalComponent(__VLS_56, new __VLS_56({
    prop: "businessLicense",
    label: "许可证编号",
    minWidth: "180",
}));
const __VLS_58 = __VLS_57({
    prop: "businessLicense",
    label: "许可证编号",
    minWidth: "180",
}, ...__VLS_functionalComponentArgsRest(__VLS_57));
const __VLS_60 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_61 = __VLS_asFunctionalComponent(__VLS_60, new __VLS_60({
    prop: "businessLicenseStart",
    label: "许可证开始",
    width: "140",
}));
const __VLS_62 = __VLS_61({
    prop: "businessLicenseStart",
    label: "许可证开始",
    width: "140",
}, ...__VLS_functionalComponentArgsRest(__VLS_61));
__VLS_63.slots.default;
{
    const { default: __VLS_thisSlot } = __VLS_63.slots;
    const [{ row }] = __VLS_getSlotParams(__VLS_thisSlot);
    (row.businessLicenseStart || "-");
}
var __VLS_63;
const __VLS_64 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_65 = __VLS_asFunctionalComponent(__VLS_64, new __VLS_64({
    prop: "businessLicenseEnd",
    label: "许可证截止",
    width: "140",
}));
const __VLS_66 = __VLS_65({
    prop: "businessLicenseEnd",
    label: "许可证截止",
    width: "140",
}, ...__VLS_functionalComponentArgsRest(__VLS_65));
__VLS_67.slots.default;
{
    const { default: __VLS_thisSlot } = __VLS_67.slots;
    const [{ row }] = __VLS_getSlotParams(__VLS_thisSlot);
    (row.businessLicenseEnd || "-");
}
var __VLS_67;
const __VLS_68 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_69 = __VLS_asFunctionalComponent(__VLS_68, new __VLS_68({
    prop: "businessLicenseIssuingAuthority",
    label: "发证机关",
    minWidth: "160",
}));
const __VLS_70 = __VLS_69({
    prop: "businessLicenseIssuingAuthority",
    label: "发证机关",
    minWidth: "160",
}, ...__VLS_functionalComponentArgsRest(__VLS_69));
const __VLS_72 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_73 = __VLS_asFunctionalComponent(__VLS_72, new __VLS_72({
    label: "许可证附件",
    width: "140",
}));
const __VLS_74 = __VLS_73({
    label: "许可证附件",
    width: "140",
}, ...__VLS_functionalComponentArgsRest(__VLS_73));
__VLS_75.slots.default;
{
    const { default: __VLS_thisSlot } = __VLS_75.slots;
    const [{ row }] = __VLS_getSlotParams(__VLS_thisSlot);
    if (row.businessLicenseFileId) {
        const __VLS_76 = {}.ElLink;
        /** @type {[typeof __VLS_components.ElLink, typeof __VLS_components.elLink, typeof __VLS_components.ElLink, typeof __VLS_components.elLink, ]} */ ;
        // @ts-ignore
        const __VLS_77 = __VLS_asFunctionalComponent(__VLS_76, new __VLS_76({
            href: (__VLS_ctx.fileUrl(row.businessLicenseFileId)),
            target: "_blank",
        }));
        const __VLS_78 = __VLS_77({
            href: (__VLS_ctx.fileUrl(row.businessLicenseFileId)),
            target: "_blank",
        }, ...__VLS_functionalComponentArgsRest(__VLS_77));
        __VLS_79.slots.default;
        var __VLS_79;
    }
    else {
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({
            ...{ class: "sf-muted" },
        });
    }
}
var __VLS_75;
const __VLS_80 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_81 = __VLS_asFunctionalComponent(__VLS_80, new __VLS_80({
    label: "操作",
    width: "180",
    fixed: "right",
}));
const __VLS_82 = __VLS_81({
    label: "操作",
    width: "180",
    fixed: "right",
}, ...__VLS_functionalComponentArgsRest(__VLS_81));
__VLS_83.slots.default;
{
    const { default: __VLS_thisSlot } = __VLS_83.slots;
    const [{ row }] = __VLS_getSlotParams(__VLS_thisSlot);
    const __VLS_84 = {}.ElButton;
    /** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
    // @ts-ignore
    const __VLS_85 = __VLS_asFunctionalComponent(__VLS_84, new __VLS_84({
        ...{ 'onClick': {} },
        size: "small",
    }));
    const __VLS_86 = __VLS_85({
        ...{ 'onClick': {} },
        size: "small",
    }, ...__VLS_functionalComponentArgsRest(__VLS_85));
    let __VLS_88;
    let __VLS_89;
    let __VLS_90;
    const __VLS_91 = {
        onClick: (...[$event]) => {
            __VLS_ctx.openEdit(row);
        }
    };
    __VLS_87.slots.default;
    var __VLS_87;
    const __VLS_92 = {}.ElButton;
    /** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
    // @ts-ignore
    const __VLS_93 = __VLS_asFunctionalComponent(__VLS_92, new __VLS_92({
        ...{ 'onClick': {} },
        size: "small",
        type: "danger",
        plain: true,
    }));
    const __VLS_94 = __VLS_93({
        ...{ 'onClick': {} },
        size: "small",
        type: "danger",
        plain: true,
    }, ...__VLS_functionalComponentArgsRest(__VLS_93));
    let __VLS_96;
    let __VLS_97;
    let __VLS_98;
    const __VLS_99 = {
        onClick: (...[$event]) => {
            __VLS_ctx.onDelete(row.id);
        }
    };
    __VLS_95.slots.default;
    var __VLS_95;
}
var __VLS_83;
var __VLS_31;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ style: {} },
});
const __VLS_100 = {}.ElPagination;
/** @type {[typeof __VLS_components.ElPagination, typeof __VLS_components.elPagination, ]} */ ;
// @ts-ignore
const __VLS_101 = __VLS_asFunctionalComponent(__VLS_100, new __VLS_100({
    ...{ 'onChange': {} },
    background: true,
    layout: "prev, pager, next, sizes, total",
    total: (__VLS_ctx.total),
    currentPage: (__VLS_ctx.page),
    pageSize: (__VLS_ctx.pageSize),
}));
const __VLS_102 = __VLS_101({
    ...{ 'onChange': {} },
    background: true,
    layout: "prev, pager, next, sizes, total",
    total: (__VLS_ctx.total),
    currentPage: (__VLS_ctx.page),
    pageSize: (__VLS_ctx.pageSize),
}, ...__VLS_functionalComponentArgsRest(__VLS_101));
let __VLS_104;
let __VLS_105;
let __VLS_106;
const __VLS_107 = {
    onChange: (__VLS_ctx.load)
};
var __VLS_103;
const __VLS_108 = {}.ElDialog;
/** @type {[typeof __VLS_components.ElDialog, typeof __VLS_components.elDialog, typeof __VLS_components.ElDialog, typeof __VLS_components.elDialog, ]} */ ;
// @ts-ignore
const __VLS_109 = __VLS_asFunctionalComponent(__VLS_108, new __VLS_108({
    modelValue: (__VLS_ctx.dialogVisible),
    title: (__VLS_ctx.editing?.id ? '编辑企业' : '新增企业'),
    width: "880px",
}));
const __VLS_110 = __VLS_109({
    modelValue: (__VLS_ctx.dialogVisible),
    title: (__VLS_ctx.editing?.id ? '编辑企业' : '新增企业'),
    width: "880px",
}, ...__VLS_functionalComponentArgsRest(__VLS_109));
__VLS_111.slots.default;
const __VLS_112 = {}.ElForm;
/** @type {[typeof __VLS_components.ElForm, typeof __VLS_components.elForm, typeof __VLS_components.ElForm, typeof __VLS_components.elForm, ]} */ ;
// @ts-ignore
const __VLS_113 = __VLS_asFunctionalComponent(__VLS_112, new __VLS_112({
    labelWidth: "120px",
}));
const __VLS_114 = __VLS_113({
    labelWidth: "120px",
}, ...__VLS_functionalComponentArgsRest(__VLS_113));
__VLS_115.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-form-grid" },
});
const __VLS_116 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_117 = __VLS_asFunctionalComponent(__VLS_116, new __VLS_116({
    label: "企业编码",
    required: true,
}));
const __VLS_118 = __VLS_117({
    label: "企业编码",
    required: true,
}, ...__VLS_functionalComponentArgsRest(__VLS_117));
__VLS_119.slots.default;
const __VLS_120 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_121 = __VLS_asFunctionalComponent(__VLS_120, new __VLS_120({
    modelValue: (__VLS_ctx.form.companyCode),
    disabled: (Boolean(__VLS_ctx.editing?.id)),
}));
const __VLS_122 = __VLS_121({
    modelValue: (__VLS_ctx.form.companyCode),
    disabled: (Boolean(__VLS_ctx.editing?.id)),
}, ...__VLS_functionalComponentArgsRest(__VLS_121));
var __VLS_119;
const __VLS_124 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_125 = __VLS_asFunctionalComponent(__VLS_124, new __VLS_124({
    label: "企业名称",
    required: true,
}));
const __VLS_126 = __VLS_125({
    label: "企业名称",
    required: true,
}, ...__VLS_functionalComponentArgsRest(__VLS_125));
__VLS_127.slots.default;
const __VLS_128 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_129 = __VLS_asFunctionalComponent(__VLS_128, new __VLS_128({
    modelValue: (__VLS_ctx.form.companyName),
}));
const __VLS_130 = __VLS_129({
    modelValue: (__VLS_ctx.form.companyName),
}, ...__VLS_functionalComponentArgsRest(__VLS_129));
var __VLS_127;
const __VLS_132 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_133 = __VLS_asFunctionalComponent(__VLS_132, new __VLS_132({
    label: "信用代码",
}));
const __VLS_134 = __VLS_133({
    label: "信用代码",
}, ...__VLS_functionalComponentArgsRest(__VLS_133));
__VLS_135.slots.default;
const __VLS_136 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_137 = __VLS_asFunctionalComponent(__VLS_136, new __VLS_136({
    modelValue: (__VLS_ctx.form.creditCode),
}));
const __VLS_138 = __VLS_137({
    modelValue: (__VLS_ctx.form.creditCode),
}, ...__VLS_functionalComponentArgsRest(__VLS_137));
var __VLS_135;
const __VLS_140 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_141 = __VLS_asFunctionalComponent(__VLS_140, new __VLS_140({
    label: "主要负责人",
}));
const __VLS_142 = __VLS_141({
    label: "主要负责人",
}, ...__VLS_functionalComponentArgsRest(__VLS_141));
__VLS_143.slots.default;
const __VLS_144 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_145 = __VLS_asFunctionalComponent(__VLS_144, new __VLS_144({
    modelValue: (__VLS_ctx.form.principalName),
}));
const __VLS_146 = __VLS_145({
    modelValue: (__VLS_ctx.form.principalName),
}, ...__VLS_functionalComponentArgsRest(__VLS_145));
var __VLS_143;
const __VLS_148 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_149 = __VLS_asFunctionalComponent(__VLS_148, new __VLS_148({
    label: "企业状态",
}));
const __VLS_150 = __VLS_149({
    label: "企业状态",
}, ...__VLS_functionalComponentArgsRest(__VLS_149));
__VLS_151.slots.default;
const __VLS_152 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_153 = __VLS_asFunctionalComponent(__VLS_152, new __VLS_152({
    modelValue: (__VLS_ctx.form.companyStatus),
    placeholder: "在业/停产…",
}));
const __VLS_154 = __VLS_153({
    modelValue: (__VLS_ctx.form.companyStatus),
    placeholder: "在业/停产…",
}, ...__VLS_functionalComponentArgsRest(__VLS_153));
var __VLS_151;
const __VLS_156 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_157 = __VLS_asFunctionalComponent(__VLS_156, new __VLS_156({
    label: "许可证编号",
}));
const __VLS_158 = __VLS_157({
    label: "许可证编号",
}, ...__VLS_functionalComponentArgsRest(__VLS_157));
__VLS_159.slots.default;
const __VLS_160 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_161 = __VLS_asFunctionalComponent(__VLS_160, new __VLS_160({
    modelValue: (__VLS_ctx.form.businessLicense),
}));
const __VLS_162 = __VLS_161({
    modelValue: (__VLS_ctx.form.businessLicense),
}, ...__VLS_functionalComponentArgsRest(__VLS_161));
var __VLS_159;
const __VLS_164 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_165 = __VLS_asFunctionalComponent(__VLS_164, new __VLS_164({
    label: "许可证开始",
}));
const __VLS_166 = __VLS_165({
    label: "许可证开始",
}, ...__VLS_functionalComponentArgsRest(__VLS_165));
__VLS_167.slots.default;
const __VLS_168 = {}.ElDatePicker;
/** @type {[typeof __VLS_components.ElDatePicker, typeof __VLS_components.elDatePicker, ]} */ ;
// @ts-ignore
const __VLS_169 = __VLS_asFunctionalComponent(__VLS_168, new __VLS_168({
    modelValue: (__VLS_ctx.form.businessLicenseStart),
    type: "date",
    valueFormat: "YYYY-MM-DD",
    placeholder: "选择日期",
    ...{ style: {} },
}));
const __VLS_170 = __VLS_169({
    modelValue: (__VLS_ctx.form.businessLicenseStart),
    type: "date",
    valueFormat: "YYYY-MM-DD",
    placeholder: "选择日期",
    ...{ style: {} },
}, ...__VLS_functionalComponentArgsRest(__VLS_169));
var __VLS_167;
const __VLS_172 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_173 = __VLS_asFunctionalComponent(__VLS_172, new __VLS_172({
    label: "许可证截止",
}));
const __VLS_174 = __VLS_173({
    label: "许可证截止",
}, ...__VLS_functionalComponentArgsRest(__VLS_173));
__VLS_175.slots.default;
const __VLS_176 = {}.ElDatePicker;
/** @type {[typeof __VLS_components.ElDatePicker, typeof __VLS_components.elDatePicker, ]} */ ;
// @ts-ignore
const __VLS_177 = __VLS_asFunctionalComponent(__VLS_176, new __VLS_176({
    modelValue: (__VLS_ctx.form.businessLicenseEnd),
    type: "date",
    valueFormat: "YYYY-MM-DD",
    placeholder: "选择日期",
    ...{ style: {} },
}));
const __VLS_178 = __VLS_177({
    modelValue: (__VLS_ctx.form.businessLicenseEnd),
    type: "date",
    valueFormat: "YYYY-MM-DD",
    placeholder: "选择日期",
    ...{ style: {} },
}, ...__VLS_functionalComponentArgsRest(__VLS_177));
var __VLS_175;
const __VLS_180 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_181 = __VLS_asFunctionalComponent(__VLS_180, new __VLS_180({
    label: "许可证附件",
    ...{ class: "sf-form-full" },
}));
const __VLS_182 = __VLS_181({
    label: "许可证附件",
    ...{ class: "sf-form-full" },
}, ...__VLS_functionalComponentArgsRest(__VLS_181));
__VLS_183.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ style: {} },
});
const __VLS_184 = {}.ElUpload;
/** @type {[typeof __VLS_components.ElUpload, typeof __VLS_components.elUpload, typeof __VLS_components.ElUpload, typeof __VLS_components.elUpload, ]} */ ;
// @ts-ignore
const __VLS_185 = __VLS_asFunctionalComponent(__VLS_184, new __VLS_184({
    showFileList: (false),
    beforeUpload: (__VLS_ctx.beforeUpload),
    accept: ".jpg,.jpeg,image/jpeg",
}));
const __VLS_186 = __VLS_185({
    showFileList: (false),
    beforeUpload: (__VLS_ctx.beforeUpload),
    accept: ".jpg,.jpeg,image/jpeg",
}, ...__VLS_functionalComponentArgsRest(__VLS_185));
__VLS_187.slots.default;
const __VLS_188 = {}.ElButton;
/** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
// @ts-ignore
const __VLS_189 = __VLS_asFunctionalComponent(__VLS_188, new __VLS_188({}));
const __VLS_190 = __VLS_189({}, ...__VLS_functionalComponentArgsRest(__VLS_189));
__VLS_191.slots.default;
var __VLS_191;
var __VLS_187;
if (__VLS_ctx.form.businessLicenseFileId) {
    const __VLS_192 = {}.ElLink;
    /** @type {[typeof __VLS_components.ElLink, typeof __VLS_components.elLink, typeof __VLS_components.ElLink, typeof __VLS_components.elLink, ]} */ ;
    // @ts-ignore
    const __VLS_193 = __VLS_asFunctionalComponent(__VLS_192, new __VLS_192({
        href: (__VLS_ctx.fileUrl(__VLS_ctx.form.businessLicenseFileId)),
        target: "_blank",
    }));
    const __VLS_194 = __VLS_193({
        href: (__VLS_ctx.fileUrl(__VLS_ctx.form.businessLicenseFileId)),
        target: "_blank",
    }, ...__VLS_functionalComponentArgsRest(__VLS_193));
    __VLS_195.slots.default;
    var __VLS_195;
}
else {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({
        ...{ class: "sf-muted" },
    });
}
var __VLS_183;
const __VLS_196 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_197 = __VLS_asFunctionalComponent(__VLS_196, new __VLS_196({
    label: "经营范围",
    ...{ class: "sf-form-full" },
}));
const __VLS_198 = __VLS_197({
    label: "经营范围",
    ...{ class: "sf-form-full" },
}, ...__VLS_functionalComponentArgsRest(__VLS_197));
__VLS_199.slots.default;
const __VLS_200 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_201 = __VLS_asFunctionalComponent(__VLS_200, new __VLS_200({
    modelValue: (__VLS_ctx.form.businessLicenseScope),
    type: "textarea",
    rows: (2),
}));
const __VLS_202 = __VLS_201({
    modelValue: (__VLS_ctx.form.businessLicenseScope),
    type: "textarea",
    rows: (2),
}, ...__VLS_functionalComponentArgsRest(__VLS_201));
var __VLS_199;
const __VLS_204 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_205 = __VLS_asFunctionalComponent(__VLS_204, new __VLS_204({
    label: "发证机关",
}));
const __VLS_206 = __VLS_205({
    label: "发证机关",
}, ...__VLS_functionalComponentArgsRest(__VLS_205));
__VLS_207.slots.default;
const __VLS_208 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_209 = __VLS_asFunctionalComponent(__VLS_208, new __VLS_208({
    modelValue: (__VLS_ctx.form.businessLicenseIssuingAuthority),
}));
const __VLS_210 = __VLS_209({
    modelValue: (__VLS_ctx.form.businessLicenseIssuingAuthority),
}, ...__VLS_functionalComponentArgsRest(__VLS_209));
var __VLS_207;
const __VLS_212 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_213 = __VLS_asFunctionalComponent(__VLS_212, new __VLS_212({
    label: "注册地址",
}));
const __VLS_214 = __VLS_213({
    label: "注册地址",
}, ...__VLS_functionalComponentArgsRest(__VLS_213));
__VLS_215.slots.default;
const __VLS_216 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_217 = __VLS_asFunctionalComponent(__VLS_216, new __VLS_216({
    modelValue: (__VLS_ctx.form.registerAddress),
}));
const __VLS_218 = __VLS_217({
    modelValue: (__VLS_ctx.form.registerAddress),
}, ...__VLS_functionalComponentArgsRest(__VLS_217));
var __VLS_215;
const __VLS_220 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_221 = __VLS_asFunctionalComponent(__VLS_220, new __VLS_220({
    label: "仓储地址",
}));
const __VLS_222 = __VLS_221({
    label: "仓储地址",
}, ...__VLS_functionalComponentArgsRest(__VLS_221));
__VLS_223.slots.default;
const __VLS_224 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_225 = __VLS_asFunctionalComponent(__VLS_224, new __VLS_224({
    modelValue: (__VLS_ctx.form.storageAddress),
}));
const __VLS_226 = __VLS_225({
    modelValue: (__VLS_ctx.form.storageAddress),
}, ...__VLS_functionalComponentArgsRest(__VLS_225));
var __VLS_223;
const __VLS_228 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_229 = __VLS_asFunctionalComponent(__VLS_228, new __VLS_228({
    label: "企业地址",
}));
const __VLS_230 = __VLS_229({
    label: "企业地址",
}, ...__VLS_functionalComponentArgsRest(__VLS_229));
__VLS_231.slots.default;
const __VLS_232 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_233 = __VLS_asFunctionalComponent(__VLS_232, new __VLS_232({
    modelValue: (__VLS_ctx.form.address),
}));
const __VLS_234 = __VLS_233({
    modelValue: (__VLS_ctx.form.address),
}, ...__VLS_functionalComponentArgsRest(__VLS_233));
var __VLS_231;
const __VLS_236 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_237 = __VLS_asFunctionalComponent(__VLS_236, new __VLS_236({
    label: "库房面积",
}));
const __VLS_238 = __VLS_237({
    label: "库房面积",
}, ...__VLS_functionalComponentArgsRest(__VLS_237));
__VLS_239.slots.default;
const __VLS_240 = {}.ElInputNumber;
/** @type {[typeof __VLS_components.ElInputNumber, typeof __VLS_components.elInputNumber, ]} */ ;
// @ts-ignore
const __VLS_241 = __VLS_asFunctionalComponent(__VLS_240, new __VLS_240({
    modelValue: (__VLS_ctx.form.storeroomArea),
    ...{ style: {} },
}));
const __VLS_242 = __VLS_241({
    modelValue: (__VLS_ctx.form.storeroomArea),
    ...{ style: {} },
}, ...__VLS_functionalComponentArgsRest(__VLS_241));
var __VLS_239;
const __VLS_244 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_245 = __VLS_asFunctionalComponent(__VLS_244, new __VLS_244({
    label: "库区面积",
}));
const __VLS_246 = __VLS_245({
    label: "库区面积",
}, ...__VLS_functionalComponentArgsRest(__VLS_245));
__VLS_247.slots.default;
const __VLS_248 = {}.ElInputNumber;
/** @type {[typeof __VLS_components.ElInputNumber, typeof __VLS_components.elInputNumber, ]} */ ;
// @ts-ignore
const __VLS_249 = __VLS_asFunctionalComponent(__VLS_248, new __VLS_248({
    modelValue: (__VLS_ctx.form.reservoirArea),
    ...{ style: {} },
}));
const __VLS_250 = __VLS_249({
    modelValue: (__VLS_ctx.form.reservoirArea),
    ...{ style: {} },
}, ...__VLS_functionalComponentArgsRest(__VLS_249));
var __VLS_247;
const __VLS_252 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_253 = __VLS_asFunctionalComponent(__VLS_252, new __VLS_252({
    label: "核定药量",
}));
const __VLS_254 = __VLS_253({
    label: "核定药量",
}, ...__VLS_functionalComponentArgsRest(__VLS_253));
__VLS_255.slots.default;
const __VLS_256 = {}.ElInputNumber;
/** @type {[typeof __VLS_components.ElInputNumber, typeof __VLS_components.elInputNumber, ]} */ ;
// @ts-ignore
const __VLS_257 = __VLS_asFunctionalComponent(__VLS_256, new __VLS_256({
    modelValue: (__VLS_ctx.form.dosage),
    ...{ style: {} },
}));
const __VLS_258 = __VLS_257({
    modelValue: (__VLS_ctx.form.dosage),
    ...{ style: {} },
}, ...__VLS_functionalComponentArgsRest(__VLS_257));
var __VLS_255;
const __VLS_260 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_261 = __VLS_asFunctionalComponent(__VLS_260, new __VLS_260({
    label: "经度",
}));
const __VLS_262 = __VLS_261({
    label: "经度",
}, ...__VLS_functionalComponentArgsRest(__VLS_261));
__VLS_263.slots.default;
const __VLS_264 = {}.ElInputNumber;
/** @type {[typeof __VLS_components.ElInputNumber, typeof __VLS_components.elInputNumber, ]} */ ;
// @ts-ignore
const __VLS_265 = __VLS_asFunctionalComponent(__VLS_264, new __VLS_264({
    modelValue: (__VLS_ctx.form.longitude),
    ...{ style: {} },
}));
const __VLS_266 = __VLS_265({
    modelValue: (__VLS_ctx.form.longitude),
    ...{ style: {} },
}, ...__VLS_functionalComponentArgsRest(__VLS_265));
var __VLS_263;
const __VLS_268 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_269 = __VLS_asFunctionalComponent(__VLS_268, new __VLS_268({
    label: "纬度",
}));
const __VLS_270 = __VLS_269({
    label: "纬度",
}, ...__VLS_functionalComponentArgsRest(__VLS_269));
__VLS_271.slots.default;
const __VLS_272 = {}.ElInputNumber;
/** @type {[typeof __VLS_components.ElInputNumber, typeof __VLS_components.elInputNumber, ]} */ ;
// @ts-ignore
const __VLS_273 = __VLS_asFunctionalComponent(__VLS_272, new __VLS_272({
    modelValue: (__VLS_ctx.form.latitude),
    ...{ style: {} },
}));
const __VLS_274 = __VLS_273({
    modelValue: (__VLS_ctx.form.latitude),
    ...{ style: {} },
}, ...__VLS_functionalComponentArgsRest(__VLS_273));
var __VLS_271;
var __VLS_115;
{
    const { footer: __VLS_thisSlot } = __VLS_111.slots;
    const __VLS_276 = {}.ElButton;
    /** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
    // @ts-ignore
    const __VLS_277 = __VLS_asFunctionalComponent(__VLS_276, new __VLS_276({
        ...{ 'onClick': {} },
    }));
    const __VLS_278 = __VLS_277({
        ...{ 'onClick': {} },
    }, ...__VLS_functionalComponentArgsRest(__VLS_277));
    let __VLS_280;
    let __VLS_281;
    let __VLS_282;
    const __VLS_283 = {
        onClick: (...[$event]) => {
            __VLS_ctx.dialogVisible = false;
        }
    };
    __VLS_279.slots.default;
    var __VLS_279;
    const __VLS_284 = {}.ElButton;
    /** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
    // @ts-ignore
    const __VLS_285 = __VLS_asFunctionalComponent(__VLS_284, new __VLS_284({
        ...{ 'onClick': {} },
        type: "primary",
        loading: (__VLS_ctx.saving),
    }));
    const __VLS_286 = __VLS_285({
        ...{ 'onClick': {} },
        type: "primary",
        loading: (__VLS_ctx.saving),
    }, ...__VLS_functionalComponentArgsRest(__VLS_285));
    let __VLS_288;
    let __VLS_289;
    let __VLS_290;
    const __VLS_291 = {
        onClick: (__VLS_ctx.onSave)
    };
    __VLS_287.slots.default;
    var __VLS_287;
}
var __VLS_111;
/** @type {__VLS_StyleScopedClasses['sf-page']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-page-head']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-page-title']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-page-sub']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-page-actions']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-card']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-table-card']} */ ;
/** @type {__VLS_StyleScopedClasses['company-detail']} */ ;
/** @type {__VLS_StyleScopedClasses['detail-item']} */ ;
/** @type {__VLS_StyleScopedClasses['label']} */ ;
/** @type {__VLS_StyleScopedClasses['detail-item']} */ ;
/** @type {__VLS_StyleScopedClasses['label']} */ ;
/** @type {__VLS_StyleScopedClasses['detail-item']} */ ;
/** @type {__VLS_StyleScopedClasses['label']} */ ;
/** @type {__VLS_StyleScopedClasses['detail-item']} */ ;
/** @type {__VLS_StyleScopedClasses['label']} */ ;
/** @type {__VLS_StyleScopedClasses['detail-item']} */ ;
/** @type {__VLS_StyleScopedClasses['label']} */ ;
/** @type {__VLS_StyleScopedClasses['detail-item']} */ ;
/** @type {__VLS_StyleScopedClasses['label']} */ ;
/** @type {__VLS_StyleScopedClasses['detail-item']} */ ;
/** @type {__VLS_StyleScopedClasses['label']} */ ;
/** @type {__VLS_StyleScopedClasses['detail-item']} */ ;
/** @type {__VLS_StyleScopedClasses['label']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-muted']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-form-grid']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-form-full']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-muted']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-form-full']} */ ;
var __VLS_dollars;
const __VLS_self = (await import('vue')).defineComponent({
    setup() {
        return {
            fileUrl: fileUrl,
            keyword: keyword,
            page: page,
            pageSize: pageSize,
            total: total,
            loading: loading,
            list: list,
            dialogVisible: dialogVisible,
            saving: saving,
            editing: editing,
            form: form,
            load: load,
            openCreate: openCreate,
            openEdit: openEdit,
            beforeUpload: beforeUpload,
            onSave: onSave,
            onDelete: onDelete,
            importCompanies: importCompanies,
        };
    },
});
export default (await import('vue')).defineComponent({
    setup() {
        return {};
    },
});
; /* PartiallyEnd: #4569/main.vue */
