import { reactive, ref, watch } from "vue";
import { ElMessage, ElMessageBox } from "element-plus";
import { fileUrl, http, rawHttp, uploadFile } from "@/api/http";
const tab = ref("people");
const keyword = ref("");
const loadingPeople = ref(false);
const people = ref([]);
const peopleTotal = ref(0);
const peoplePage = ref(1);
const peoplePageSize = ref(20);
const loadingRecords = ref(false);
const records = ref([]);
const recordsTotal = ref(0);
const recordsPage = ref(1);
const recordsPageSize = ref(20);
const recordDialogVisible = ref(false);
const recordSaving = ref(false);
const recordEditing = ref(null);
const recordForm = reactive({
    companyCode: "",
    personName: "",
    idcard: "",
    personType: "",
    inOutState: "IN",
    inOutTime: "",
    imageFileId: null
});
const dialogVisible = ref(false);
const saving = ref(false);
const editing = ref(null);
const form = reactive({
    companyCode: "",
    personName: "",
    idcard: "",
    personType: "",
    isCertified: 0,
    phone: "",
    avatarFileId: null,
    certFileId: null,
    certExpireDate: null,
    smsNotify: 0
});
function fmt(ts) {
    const d = new Date(ts);
    const pad = (n) => String(n).padStart(2, "0");
    return `${d.getFullYear()}-${pad(d.getMonth() + 1)}-${pad(d.getDate())} ${pad(d.getHours())}:${pad(d.getMinutes())}:${pad(d.getSeconds())}`;
}
async function loadPeople() {
    loadingPeople.value = true;
    try {
        const data = await http.get(`/api/v1/people?page=${peoplePage.value}&pageSize=${peoplePageSize.value}${keyword.value ? `&keyword=${encodeURIComponent(keyword.value)}` : ""}`);
        people.value = data.list;
        peopleTotal.value = data.total;
    }
    catch (e) {
        ElMessage.error(e?.message || "加载失败");
    }
    finally {
        loadingPeople.value = false;
    }
}
async function loadRecords() {
    loadingRecords.value = true;
    try {
        const data = await http.get(`/api/v1/person-inout?page=${recordsPage.value}&pageSize=${recordsPageSize.value}`);
        records.value = data.list;
        recordsTotal.value = data.total;
    }
    catch (e) {
        ElMessage.error(e?.message || "加载失败");
    }
    finally {
        loadingRecords.value = false;
    }
}
function openRecordCreate() {
    recordEditing.value = null;
    Object.assign(recordForm, {
        companyCode: "",
        personName: "",
        idcard: "",
        personType: "",
        inOutState: "IN",
        inOutTime: "",
        imageFileId: null
    });
    recordDialogVisible.value = true;
}
function openRecordEdit(row) {
    recordEditing.value = row;
    Object.assign(recordForm, {
        companyCode: row.companyCode || "",
        personName: row.personName,
        idcard: row.idcard,
        personType: row.personType,
        inOutState: row.inOutState,
        inOutTime: String(row.inOutTime),
        imageFileId: row.imageFileId || null
    });
    recordDialogVisible.value = true;
}
async function uploadRecordImage(file) {
    if (!isJpg(file)) {
        ElMessage.warning("请上传 JPG 图片");
        return false;
    }
    try {
        const out = await uploadFile("person_inout", file);
        recordForm.imageFileId = out.id;
        ElMessage.success("上传成功");
    }
    catch (e) {
        ElMessage.error(e?.message || "上传失败");
    }
    return false;
}
async function saveRecord() {
    if (!recordForm.personName || !recordForm.idcard || !recordForm.personType || !recordForm.inOutTime) {
        ElMessage.warning("请补全必填项");
        return;
    }
    recordSaving.value = true;
    try {
        const payload = {
            id: recordEditing.value?.id,
            companyCode: recordForm.companyCode || null,
            personName: recordForm.personName,
            idcard: recordForm.idcard,
            personType: recordForm.personType,
            inOutState: recordForm.inOutState,
            inOutTime: Number(recordForm.inOutTime),
            imageFileId: recordForm.imageFileId
        };
        if (recordEditing.value?.id) {
            await http.put("/api/v1/person-inout", payload);
        }
        else {
            await http.post("/api/v1/person-inout", payload);
        }
        ElMessage.success("保存成功");
        recordDialogVisible.value = false;
        await loadRecords();
    }
    catch (e) {
        ElMessage.error(e?.message || "保存失败");
    }
    finally {
        recordSaving.value = false;
    }
}
async function deleteRecord(id) {
    const ok = await ElMessageBox.confirm("确认删除该记录？", "提示", { type: "warning" }).catch(() => null);
    if (!ok)
        return;
    try {
        await http.del(`/api/v1/person-inout/${id}`);
        ElMessage.success("删除成功");
        await loadRecords();
    }
    catch (e) {
        ElMessage.error(e?.message || "删除失败");
    }
}
async function importPeople(file) {
    const fd = new FormData();
    fd.append("file", file);
    try {
        const res = await rawHttp.post("/api/v1/people/import", fd, {
            headers: { "Content-Type": "multipart/form-data" }
        });
        ElMessage.success(`导入完成：成功 ${res.data.successCount} 条，失败 ${res.data.failCount} 条`);
        await loadPeople();
    }
    catch (e) {
        ElMessage.error(e?.message || "导入失败");
    }
    return false;
}
function openCreate() {
    editing.value = null;
    Object.assign(form, {
        companyCode: "",
        personName: "",
        idcard: "",
        personType: "",
        isCertified: 0,
        phone: "",
        avatarFileId: null,
        certFileId: null,
        certExpireDate: null,
        smsNotify: 0
    });
    dialogVisible.value = true;
}
function openEdit(row) {
    editing.value = row;
    Object.assign(form, {
        companyCode: row.companyCode,
        personName: row.personName,
        idcard: "",
        personType: row.personType,
        isCertified: row.isCertified,
        phone: row.phone || "",
        avatarFileId: row.avatarFileId || null,
        certFileId: row.certFileId || null,
        certExpireDate: row.certExpireDate || null,
        smsNotify: row.smsNotify ?? 0
    });
    dialogVisible.value = true;
}
function isJpg(file) {
    const name = file.name.toLowerCase();
    return file.type === "image/jpeg" || name.endsWith(".jpg") || name.endsWith(".jpeg");
}
async function uploadPersonFile(file, bizType) {
    if (!isJpg(file)) {
        ElMessage.warning("请上传 JPG 图片");
        return false;
    }
    try {
        const out = await uploadFile(bizType, file);
        if (bizType === "avatar") {
            form.avatarFileId = out.id;
        }
        else {
            form.certFileId = out.id;
        }
        ElMessage.success("上传成功");
    }
    catch (e) {
        ElMessage.error(e?.message || "上传失败");
    }
    return false;
}
async function onSave() {
    if (!form.companyCode || !form.personName || (!editing.value?.id && !form.idcard) || !form.personType) {
        ElMessage.warning("请补全必填项");
        return;
    }
    saving.value = true;
    try {
        if (!editing.value?.id) {
            await http.post("/api/v1/people", { ...form });
        }
        else {
            await http.put("/api/v1/people", { id: editing.value.id, ...form });
        }
        ElMessage.success("保存成功");
        dialogVisible.value = false;
        await loadPeople();
    }
    catch (e) {
        ElMessage.error(e?.message || "保存失败");
    }
    finally {
        saving.value = false;
    }
}
async function onDelete(id) {
    const ok = await ElMessageBox.confirm("确认删除该人员？", "提示", { type: "warning" }).catch(() => null);
    if (!ok)
        return;
    try {
        await http.del(`/api/v1/people/${id}`);
        ElMessage.success("删除成功");
        await loadPeople();
    }
    catch (e) {
        ElMessage.error(e?.message || "删除失败");
    }
}
watch([peoplePage, peoplePageSize], () => loadPeople());
watch([recordsPage, recordsPageSize], () => loadRecords());
watch(keyword, () => {
    peoplePage.value = 1;
    loadPeople();
});
watch(tab, () => {
    if (tab.value === "records")
        loadRecords();
});
loadPeople();
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
const __VLS_0 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_1 = __VLS_asFunctionalComponent(__VLS_0, new __VLS_0({
    modelValue: (__VLS_ctx.keyword),
    placeholder: "姓名/身份证号码/企业编码",
    ...{ style: {} },
    clearable: true,
}));
const __VLS_2 = __VLS_1({
    modelValue: (__VLS_ctx.keyword),
    placeholder: "姓名/身份证号码/企业编码",
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
    beforeUpload: (__VLS_ctx.importPeople),
    accept: ".csv",
}));
const __VLS_14 = __VLS_13({
    showFileList: (false),
    beforeUpload: (__VLS_ctx.importPeople),
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
    onClick: (__VLS_ctx.loadPeople)
};
__VLS_23.slots.default;
var __VLS_23;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-card sf-table-card" },
});
const __VLS_28 = {}.ElTabs;
/** @type {[typeof __VLS_components.ElTabs, typeof __VLS_components.elTabs, typeof __VLS_components.ElTabs, typeof __VLS_components.elTabs, ]} */ ;
// @ts-ignore
const __VLS_29 = __VLS_asFunctionalComponent(__VLS_28, new __VLS_28({
    modelValue: (__VLS_ctx.tab),
}));
const __VLS_30 = __VLS_29({
    modelValue: (__VLS_ctx.tab),
}, ...__VLS_functionalComponentArgsRest(__VLS_29));
__VLS_31.slots.default;
const __VLS_32 = {}.ElTabPane;
/** @type {[typeof __VLS_components.ElTabPane, typeof __VLS_components.elTabPane, typeof __VLS_components.ElTabPane, typeof __VLS_components.elTabPane, ]} */ ;
// @ts-ignore
const __VLS_33 = __VLS_asFunctionalComponent(__VLS_32, new __VLS_32({
    label: "人员档案",
    name: "people",
}));
const __VLS_34 = __VLS_33({
    label: "人员档案",
    name: "people",
}, ...__VLS_functionalComponentArgsRest(__VLS_33));
__VLS_35.slots.default;
const __VLS_36 = {}.ElTable;
/** @type {[typeof __VLS_components.ElTable, typeof __VLS_components.elTable, typeof __VLS_components.ElTable, typeof __VLS_components.elTable, ]} */ ;
// @ts-ignore
const __VLS_37 = __VLS_asFunctionalComponent(__VLS_36, new __VLS_36({
    data: (__VLS_ctx.people),
    ...{ style: {} },
}));
const __VLS_38 = __VLS_37({
    data: (__VLS_ctx.people),
    ...{ style: {} },
}, ...__VLS_functionalComponentArgsRest(__VLS_37));
__VLS_asFunctionalDirective(__VLS_directives.vLoading)(null, { ...__VLS_directiveBindingRestFields, value: (__VLS_ctx.loadingPeople) }, null, null);
__VLS_39.slots.default;
const __VLS_40 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_41 = __VLS_asFunctionalComponent(__VLS_40, new __VLS_40({
    prop: "companyCode",
    label: "企业编码",
    minWidth: "140",
}));
const __VLS_42 = __VLS_41({
    prop: "companyCode",
    label: "企业编码",
    minWidth: "140",
}, ...__VLS_functionalComponentArgsRest(__VLS_41));
const __VLS_44 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_45 = __VLS_asFunctionalComponent(__VLS_44, new __VLS_44({
    prop: "personName",
    label: "姓名",
    width: "120",
}));
const __VLS_46 = __VLS_45({
    prop: "personName",
    label: "姓名",
    width: "120",
}, ...__VLS_functionalComponentArgsRest(__VLS_45));
const __VLS_48 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_49 = __VLS_asFunctionalComponent(__VLS_48, new __VLS_48({
    prop: "idcardMasked",
    label: "身份证号码",
    minWidth: "180",
}));
const __VLS_50 = __VLS_49({
    prop: "idcardMasked",
    label: "身份证号码",
    minWidth: "180",
}, ...__VLS_functionalComponentArgsRest(__VLS_49));
const __VLS_52 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_53 = __VLS_asFunctionalComponent(__VLS_52, new __VLS_52({
    prop: "personType",
    label: "人员类型",
    minWidth: "160",
}));
const __VLS_54 = __VLS_53({
    prop: "personType",
    label: "人员类型",
    minWidth: "160",
}, ...__VLS_functionalComponentArgsRest(__VLS_53));
const __VLS_56 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_57 = __VLS_asFunctionalComponent(__VLS_56, new __VLS_56({
    prop: "isCertified",
    label: "持证",
    width: "90",
}));
const __VLS_58 = __VLS_57({
    prop: "isCertified",
    label: "持证",
    width: "90",
}, ...__VLS_functionalComponentArgsRest(__VLS_57));
__VLS_59.slots.default;
{
    const { default: __VLS_thisSlot } = __VLS_59.slots;
    const [{ row }] = __VLS_getSlotParams(__VLS_thisSlot);
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({
        ...{ class: "sf-chip" },
        ...{ style: (row.isCertified === 1 ? 'border-color: rgba(51,209,122,0.25)' : '') },
    });
    (row.isCertified === 1 ? "是" : "否");
}
var __VLS_59;
const __VLS_60 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_61 = __VLS_asFunctionalComponent(__VLS_60, new __VLS_60({
    label: "头像照片",
    width: "110",
}));
const __VLS_62 = __VLS_61({
    label: "头像照片",
    width: "110",
}, ...__VLS_functionalComponentArgsRest(__VLS_61));
__VLS_63.slots.default;
{
    const { default: __VLS_thisSlot } = __VLS_63.slots;
    const [{ row }] = __VLS_getSlotParams(__VLS_thisSlot);
    if (row.avatarFileId) {
        const __VLS_64 = {}.ElLink;
        /** @type {[typeof __VLS_components.ElLink, typeof __VLS_components.elLink, typeof __VLS_components.ElLink, typeof __VLS_components.elLink, ]} */ ;
        // @ts-ignore
        const __VLS_65 = __VLS_asFunctionalComponent(__VLS_64, new __VLS_64({
            href: (__VLS_ctx.fileUrl(row.avatarFileId)),
            target: "_blank",
        }));
        const __VLS_66 = __VLS_65({
            href: (__VLS_ctx.fileUrl(row.avatarFileId)),
            target: "_blank",
        }, ...__VLS_functionalComponentArgsRest(__VLS_65));
        __VLS_67.slots.default;
        var __VLS_67;
    }
    else {
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({
            ...{ class: "sf-muted" },
        });
    }
}
var __VLS_63;
const __VLS_68 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_69 = __VLS_asFunctionalComponent(__VLS_68, new __VLS_68({
    label: "证书附件",
    width: "110",
}));
const __VLS_70 = __VLS_69({
    label: "证书附件",
    width: "110",
}, ...__VLS_functionalComponentArgsRest(__VLS_69));
__VLS_71.slots.default;
{
    const { default: __VLS_thisSlot } = __VLS_71.slots;
    const [{ row }] = __VLS_getSlotParams(__VLS_thisSlot);
    if (row.certFileId) {
        const __VLS_72 = {}.ElLink;
        /** @type {[typeof __VLS_components.ElLink, typeof __VLS_components.elLink, typeof __VLS_components.ElLink, typeof __VLS_components.elLink, ]} */ ;
        // @ts-ignore
        const __VLS_73 = __VLS_asFunctionalComponent(__VLS_72, new __VLS_72({
            href: (__VLS_ctx.fileUrl(row.certFileId)),
            target: "_blank",
        }));
        const __VLS_74 = __VLS_73({
            href: (__VLS_ctx.fileUrl(row.certFileId)),
            target: "_blank",
        }, ...__VLS_functionalComponentArgsRest(__VLS_73));
        __VLS_75.slots.default;
        var __VLS_75;
    }
    else {
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({
            ...{ class: "sf-muted" },
        });
    }
}
var __VLS_71;
const __VLS_76 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_77 = __VLS_asFunctionalComponent(__VLS_76, new __VLS_76({
    prop: "certExpireDate",
    label: "证书到期日期",
    width: "140",
}));
const __VLS_78 = __VLS_77({
    prop: "certExpireDate",
    label: "证书到期日期",
    width: "140",
}, ...__VLS_functionalComponentArgsRest(__VLS_77));
__VLS_79.slots.default;
{
    const { default: __VLS_thisSlot } = __VLS_79.slots;
    const [{ row }] = __VLS_getSlotParams(__VLS_thisSlot);
    (row.certExpireDate || "-");
}
var __VLS_79;
const __VLS_80 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_81 = __VLS_asFunctionalComponent(__VLS_80, new __VLS_80({
    prop: "smsNotify",
    label: "短信推送",
    width: "110",
}));
const __VLS_82 = __VLS_81({
    prop: "smsNotify",
    label: "短信推送",
    width: "110",
}, ...__VLS_functionalComponentArgsRest(__VLS_81));
__VLS_83.slots.default;
{
    const { default: __VLS_thisSlot } = __VLS_83.slots;
    const [{ row }] = __VLS_getSlotParams(__VLS_thisSlot);
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({
        ...{ class: "sf-chip" },
        ...{ style: (row.smsNotify === 1 ? 'border-color: rgba(63,120,255,0.35)' : '') },
    });
    (row.smsNotify === 1 ? "是" : "否");
}
var __VLS_83;
const __VLS_84 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_85 = __VLS_asFunctionalComponent(__VLS_84, new __VLS_84({
    prop: "phone",
    label: "手机号",
    width: "140",
}));
const __VLS_86 = __VLS_85({
    prop: "phone",
    label: "手机号",
    width: "140",
}, ...__VLS_functionalComponentArgsRest(__VLS_85));
const __VLS_88 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_89 = __VLS_asFunctionalComponent(__VLS_88, new __VLS_88({
    prop: "dataSyncTime",
    label: "同步时间",
    minWidth: "170",
}));
const __VLS_90 = __VLS_89({
    prop: "dataSyncTime",
    label: "同步时间",
    minWidth: "170",
}, ...__VLS_functionalComponentArgsRest(__VLS_89));
__VLS_91.slots.default;
{
    const { default: __VLS_thisSlot } = __VLS_91.slots;
    const [{ row }] = __VLS_getSlotParams(__VLS_thisSlot);
    (row.dataSyncTime ? __VLS_ctx.fmt(row.dataSyncTime) : "-");
}
var __VLS_91;
const __VLS_92 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_93 = __VLS_asFunctionalComponent(__VLS_92, new __VLS_92({
    label: "操作",
    width: "180",
    fixed: "right",
}));
const __VLS_94 = __VLS_93({
    label: "操作",
    width: "180",
    fixed: "right",
}, ...__VLS_functionalComponentArgsRest(__VLS_93));
__VLS_95.slots.default;
{
    const { default: __VLS_thisSlot } = __VLS_95.slots;
    const [{ row }] = __VLS_getSlotParams(__VLS_thisSlot);
    const __VLS_96 = {}.ElButton;
    /** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
    // @ts-ignore
    const __VLS_97 = __VLS_asFunctionalComponent(__VLS_96, new __VLS_96({
        ...{ 'onClick': {} },
        size: "small",
    }));
    const __VLS_98 = __VLS_97({
        ...{ 'onClick': {} },
        size: "small",
    }, ...__VLS_functionalComponentArgsRest(__VLS_97));
    let __VLS_100;
    let __VLS_101;
    let __VLS_102;
    const __VLS_103 = {
        onClick: (...[$event]) => {
            __VLS_ctx.openEdit(row);
        }
    };
    __VLS_99.slots.default;
    var __VLS_99;
    const __VLS_104 = {}.ElButton;
    /** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
    // @ts-ignore
    const __VLS_105 = __VLS_asFunctionalComponent(__VLS_104, new __VLS_104({
        ...{ 'onClick': {} },
        size: "small",
        type: "danger",
        plain: true,
    }));
    const __VLS_106 = __VLS_105({
        ...{ 'onClick': {} },
        size: "small",
        type: "danger",
        plain: true,
    }, ...__VLS_functionalComponentArgsRest(__VLS_105));
    let __VLS_108;
    let __VLS_109;
    let __VLS_110;
    const __VLS_111 = {
        onClick: (...[$event]) => {
            __VLS_ctx.onDelete(row.id);
        }
    };
    __VLS_107.slots.default;
    var __VLS_107;
}
var __VLS_95;
var __VLS_39;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ style: {} },
});
const __VLS_112 = {}.ElPagination;
/** @type {[typeof __VLS_components.ElPagination, typeof __VLS_components.elPagination, ]} */ ;
// @ts-ignore
const __VLS_113 = __VLS_asFunctionalComponent(__VLS_112, new __VLS_112({
    ...{ 'onChange': {} },
    background: true,
    layout: "prev, pager, next, sizes, total",
    total: (__VLS_ctx.peopleTotal),
    currentPage: (__VLS_ctx.peoplePage),
    pageSize: (__VLS_ctx.peoplePageSize),
}));
const __VLS_114 = __VLS_113({
    ...{ 'onChange': {} },
    background: true,
    layout: "prev, pager, next, sizes, total",
    total: (__VLS_ctx.peopleTotal),
    currentPage: (__VLS_ctx.peoplePage),
    pageSize: (__VLS_ctx.peoplePageSize),
}, ...__VLS_functionalComponentArgsRest(__VLS_113));
let __VLS_116;
let __VLS_117;
let __VLS_118;
const __VLS_119 = {
    onChange: (__VLS_ctx.loadPeople)
};
var __VLS_115;
var __VLS_35;
const __VLS_120 = {}.ElTabPane;
/** @type {[typeof __VLS_components.ElTabPane, typeof __VLS_components.elTabPane, typeof __VLS_components.ElTabPane, typeof __VLS_components.elTabPane, ]} */ ;
// @ts-ignore
const __VLS_121 = __VLS_asFunctionalComponent(__VLS_120, new __VLS_120({
    label: "出入记录",
    name: "records",
}));
const __VLS_122 = __VLS_121({
    label: "出入记录",
    name: "records",
}, ...__VLS_functionalComponentArgsRest(__VLS_121));
__VLS_123.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-filter" },
    ...{ style: {} },
});
const __VLS_124 = {}.ElButton;
/** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
// @ts-ignore
const __VLS_125 = __VLS_asFunctionalComponent(__VLS_124, new __VLS_124({
    ...{ 'onClick': {} },
    type: "primary",
}));
const __VLS_126 = __VLS_125({
    ...{ 'onClick': {} },
    type: "primary",
}, ...__VLS_functionalComponentArgsRest(__VLS_125));
let __VLS_128;
let __VLS_129;
let __VLS_130;
const __VLS_131 = {
    onClick: (__VLS_ctx.openRecordCreate)
};
__VLS_127.slots.default;
var __VLS_127;
const __VLS_132 = {}.ElButton;
/** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
// @ts-ignore
const __VLS_133 = __VLS_asFunctionalComponent(__VLS_132, new __VLS_132({
    ...{ 'onClick': {} },
}));
const __VLS_134 = __VLS_133({
    ...{ 'onClick': {} },
}, ...__VLS_functionalComponentArgsRest(__VLS_133));
let __VLS_136;
let __VLS_137;
let __VLS_138;
const __VLS_139 = {
    onClick: (__VLS_ctx.loadRecords)
};
__VLS_135.slots.default;
var __VLS_135;
const __VLS_140 = {}.ElTable;
/** @type {[typeof __VLS_components.ElTable, typeof __VLS_components.elTable, typeof __VLS_components.ElTable, typeof __VLS_components.elTable, ]} */ ;
// @ts-ignore
const __VLS_141 = __VLS_asFunctionalComponent(__VLS_140, new __VLS_140({
    data: (__VLS_ctx.records),
    ...{ style: {} },
}));
const __VLS_142 = __VLS_141({
    data: (__VLS_ctx.records),
    ...{ style: {} },
}, ...__VLS_functionalComponentArgsRest(__VLS_141));
__VLS_asFunctionalDirective(__VLS_directives.vLoading)(null, { ...__VLS_directiveBindingRestFields, value: (__VLS_ctx.loadingRecords) }, null, null);
__VLS_143.slots.default;
const __VLS_144 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_145 = __VLS_asFunctionalComponent(__VLS_144, new __VLS_144({
    prop: "companyCode",
    label: "企业编码",
    minWidth: "140",
}));
const __VLS_146 = __VLS_145({
    prop: "companyCode",
    label: "企业编码",
    minWidth: "140",
}, ...__VLS_functionalComponentArgsRest(__VLS_145));
const __VLS_148 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_149 = __VLS_asFunctionalComponent(__VLS_148, new __VLS_148({
    prop: "personName",
    label: "姓名",
    width: "120",
}));
const __VLS_150 = __VLS_149({
    prop: "personName",
    label: "姓名",
    width: "120",
}, ...__VLS_functionalComponentArgsRest(__VLS_149));
const __VLS_152 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_153 = __VLS_asFunctionalComponent(__VLS_152, new __VLS_152({
    prop: "idcardMasked",
    label: "身份证号码",
    minWidth: "180",
}));
const __VLS_154 = __VLS_153({
    prop: "idcardMasked",
    label: "身份证号码",
    minWidth: "180",
}, ...__VLS_functionalComponentArgsRest(__VLS_153));
const __VLS_156 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_157 = __VLS_asFunctionalComponent(__VLS_156, new __VLS_156({
    prop: "personType",
    label: "人员类型",
    minWidth: "140",
}));
const __VLS_158 = __VLS_157({
    prop: "personType",
    label: "人员类型",
    minWidth: "140",
}, ...__VLS_functionalComponentArgsRest(__VLS_157));
const __VLS_160 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_161 = __VLS_asFunctionalComponent(__VLS_160, new __VLS_160({
    prop: "inOutState",
    label: "出入状态",
    width: "110",
}));
const __VLS_162 = __VLS_161({
    prop: "inOutState",
    label: "出入状态",
    width: "110",
}, ...__VLS_functionalComponentArgsRest(__VLS_161));
__VLS_163.slots.default;
{
    const { default: __VLS_thisSlot } = __VLS_163.slots;
    const [{ row }] = __VLS_getSlotParams(__VLS_thisSlot);
    (row.inOutState === "IN" ? "进" : row.inOutState === "OUT" ? "出" : row.inOutState);
}
var __VLS_163;
const __VLS_164 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_165 = __VLS_asFunctionalComponent(__VLS_164, new __VLS_164({
    prop: "inOutTime",
    label: "进出时间",
    minWidth: "180",
}));
const __VLS_166 = __VLS_165({
    prop: "inOutTime",
    label: "进出时间",
    minWidth: "180",
}, ...__VLS_functionalComponentArgsRest(__VLS_165));
__VLS_167.slots.default;
{
    const { default: __VLS_thisSlot } = __VLS_167.slots;
    const [{ row }] = __VLS_getSlotParams(__VLS_thisSlot);
    (__VLS_ctx.fmt(row.inOutTime));
}
var __VLS_167;
const __VLS_168 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_169 = __VLS_asFunctionalComponent(__VLS_168, new __VLS_168({
    label: "人员照片",
    width: "110",
}));
const __VLS_170 = __VLS_169({
    label: "人员照片",
    width: "110",
}, ...__VLS_functionalComponentArgsRest(__VLS_169));
__VLS_171.slots.default;
{
    const { default: __VLS_thisSlot } = __VLS_171.slots;
    const [{ row }] = __VLS_getSlotParams(__VLS_thisSlot);
    if (row.imageFileId) {
        const __VLS_172 = {}.ElLink;
        /** @type {[typeof __VLS_components.ElLink, typeof __VLS_components.elLink, typeof __VLS_components.ElLink, typeof __VLS_components.elLink, ]} */ ;
        // @ts-ignore
        const __VLS_173 = __VLS_asFunctionalComponent(__VLS_172, new __VLS_172({
            href: (__VLS_ctx.fileUrl(row.imageFileId)),
            target: "_blank",
        }));
        const __VLS_174 = __VLS_173({
            href: (__VLS_ctx.fileUrl(row.imageFileId)),
            target: "_blank",
        }, ...__VLS_functionalComponentArgsRest(__VLS_173));
        __VLS_175.slots.default;
        var __VLS_175;
    }
    else {
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({
            ...{ class: "sf-muted" },
        });
    }
}
var __VLS_171;
const __VLS_176 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_177 = __VLS_asFunctionalComponent(__VLS_176, new __VLS_176({
    label: "操作",
    width: "160",
    fixed: "right",
}));
const __VLS_178 = __VLS_177({
    label: "操作",
    width: "160",
    fixed: "right",
}, ...__VLS_functionalComponentArgsRest(__VLS_177));
__VLS_179.slots.default;
{
    const { default: __VLS_thisSlot } = __VLS_179.slots;
    const [{ row }] = __VLS_getSlotParams(__VLS_thisSlot);
    const __VLS_180 = {}.ElButton;
    /** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
    // @ts-ignore
    const __VLS_181 = __VLS_asFunctionalComponent(__VLS_180, new __VLS_180({
        ...{ 'onClick': {} },
        size: "small",
    }));
    const __VLS_182 = __VLS_181({
        ...{ 'onClick': {} },
        size: "small",
    }, ...__VLS_functionalComponentArgsRest(__VLS_181));
    let __VLS_184;
    let __VLS_185;
    let __VLS_186;
    const __VLS_187 = {
        onClick: (...[$event]) => {
            __VLS_ctx.openRecordEdit(row);
        }
    };
    __VLS_183.slots.default;
    var __VLS_183;
    const __VLS_188 = {}.ElButton;
    /** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
    // @ts-ignore
    const __VLS_189 = __VLS_asFunctionalComponent(__VLS_188, new __VLS_188({
        ...{ 'onClick': {} },
        size: "small",
        type: "danger",
        plain: true,
    }));
    const __VLS_190 = __VLS_189({
        ...{ 'onClick': {} },
        size: "small",
        type: "danger",
        plain: true,
    }, ...__VLS_functionalComponentArgsRest(__VLS_189));
    let __VLS_192;
    let __VLS_193;
    let __VLS_194;
    const __VLS_195 = {
        onClick: (...[$event]) => {
            __VLS_ctx.deleteRecord(row.id);
        }
    };
    __VLS_191.slots.default;
    var __VLS_191;
}
var __VLS_179;
var __VLS_143;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ style: {} },
});
const __VLS_196 = {}.ElPagination;
/** @type {[typeof __VLS_components.ElPagination, typeof __VLS_components.elPagination, ]} */ ;
// @ts-ignore
const __VLS_197 = __VLS_asFunctionalComponent(__VLS_196, new __VLS_196({
    ...{ 'onChange': {} },
    background: true,
    layout: "prev, pager, next, sizes, total",
    total: (__VLS_ctx.recordsTotal),
    currentPage: (__VLS_ctx.recordsPage),
    pageSize: (__VLS_ctx.recordsPageSize),
}));
const __VLS_198 = __VLS_197({
    ...{ 'onChange': {} },
    background: true,
    layout: "prev, pager, next, sizes, total",
    total: (__VLS_ctx.recordsTotal),
    currentPage: (__VLS_ctx.recordsPage),
    pageSize: (__VLS_ctx.recordsPageSize),
}, ...__VLS_functionalComponentArgsRest(__VLS_197));
let __VLS_200;
let __VLS_201;
let __VLS_202;
const __VLS_203 = {
    onChange: (__VLS_ctx.loadRecords)
};
var __VLS_199;
var __VLS_123;
var __VLS_31;
const __VLS_204 = {}.ElDialog;
/** @type {[typeof __VLS_components.ElDialog, typeof __VLS_components.elDialog, typeof __VLS_components.ElDialog, typeof __VLS_components.elDialog, ]} */ ;
// @ts-ignore
const __VLS_205 = __VLS_asFunctionalComponent(__VLS_204, new __VLS_204({
    modelValue: (__VLS_ctx.dialogVisible),
    title: (__VLS_ctx.editing?.id ? '编辑人员' : '新增人员'),
    width: "720px",
}));
const __VLS_206 = __VLS_205({
    modelValue: (__VLS_ctx.dialogVisible),
    title: (__VLS_ctx.editing?.id ? '编辑人员' : '新增人员'),
    width: "720px",
}, ...__VLS_functionalComponentArgsRest(__VLS_205));
__VLS_207.slots.default;
const __VLS_208 = {}.ElForm;
/** @type {[typeof __VLS_components.ElForm, typeof __VLS_components.elForm, typeof __VLS_components.ElForm, typeof __VLS_components.elForm, ]} */ ;
// @ts-ignore
const __VLS_209 = __VLS_asFunctionalComponent(__VLS_208, new __VLS_208({
    labelWidth: "110px",
}));
const __VLS_210 = __VLS_209({
    labelWidth: "110px",
}, ...__VLS_functionalComponentArgsRest(__VLS_209));
__VLS_211.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-form-grid" },
});
const __VLS_212 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_213 = __VLS_asFunctionalComponent(__VLS_212, new __VLS_212({
    label: "企业编码",
    required: true,
}));
const __VLS_214 = __VLS_213({
    label: "企业编码",
    required: true,
}, ...__VLS_functionalComponentArgsRest(__VLS_213));
__VLS_215.slots.default;
const __VLS_216 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_217 = __VLS_asFunctionalComponent(__VLS_216, new __VLS_216({
    modelValue: (__VLS_ctx.form.companyCode),
}));
const __VLS_218 = __VLS_217({
    modelValue: (__VLS_ctx.form.companyCode),
}, ...__VLS_functionalComponentArgsRest(__VLS_217));
var __VLS_215;
const __VLS_220 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_221 = __VLS_asFunctionalComponent(__VLS_220, new __VLS_220({
    label: "姓名",
    required: true,
}));
const __VLS_222 = __VLS_221({
    label: "姓名",
    required: true,
}, ...__VLS_functionalComponentArgsRest(__VLS_221));
__VLS_223.slots.default;
const __VLS_224 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_225 = __VLS_asFunctionalComponent(__VLS_224, new __VLS_224({
    modelValue: (__VLS_ctx.form.personName),
}));
const __VLS_226 = __VLS_225({
    modelValue: (__VLS_ctx.form.personName),
}, ...__VLS_functionalComponentArgsRest(__VLS_225));
var __VLS_223;
if (!__VLS_ctx.editing?.id) {
    const __VLS_228 = {}.ElFormItem;
    /** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
    // @ts-ignore
    const __VLS_229 = __VLS_asFunctionalComponent(__VLS_228, new __VLS_228({
        label: "身份证号码",
        required: true,
    }));
    const __VLS_230 = __VLS_229({
        label: "身份证号码",
        required: true,
    }, ...__VLS_functionalComponentArgsRest(__VLS_229));
    __VLS_231.slots.default;
    const __VLS_232 = {}.ElInput;
    /** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
    // @ts-ignore
    const __VLS_233 = __VLS_asFunctionalComponent(__VLS_232, new __VLS_232({
        modelValue: (__VLS_ctx.form.idcard),
    }));
    const __VLS_234 = __VLS_233({
        modelValue: (__VLS_ctx.form.idcard),
    }, ...__VLS_functionalComponentArgsRest(__VLS_233));
    var __VLS_231;
}
const __VLS_236 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_237 = __VLS_asFunctionalComponent(__VLS_236, new __VLS_236({
    label: "人员类型",
    required: true,
}));
const __VLS_238 = __VLS_237({
    label: "人员类型",
    required: true,
}, ...__VLS_functionalComponentArgsRest(__VLS_237));
__VLS_239.slots.default;
const __VLS_240 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_241 = __VLS_asFunctionalComponent(__VLS_240, new __VLS_240({
    modelValue: (__VLS_ctx.form.personType),
    placeholder: "法定代表人/负责人/安全管理/特种作业/其他",
}));
const __VLS_242 = __VLS_241({
    modelValue: (__VLS_ctx.form.personType),
    placeholder: "法定代表人/负责人/安全管理/特种作业/其他",
}, ...__VLS_functionalComponentArgsRest(__VLS_241));
var __VLS_239;
const __VLS_244 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_245 = __VLS_asFunctionalComponent(__VLS_244, new __VLS_244({
    label: "持证",
}));
const __VLS_246 = __VLS_245({
    label: "持证",
}, ...__VLS_functionalComponentArgsRest(__VLS_245));
__VLS_247.slots.default;
const __VLS_248 = {}.ElSwitch;
/** @type {[typeof __VLS_components.ElSwitch, typeof __VLS_components.elSwitch, ]} */ ;
// @ts-ignore
const __VLS_249 = __VLS_asFunctionalComponent(__VLS_248, new __VLS_248({
    modelValue: (__VLS_ctx.form.isCertified),
    activeValue: (1),
    inactiveValue: (0),
}));
const __VLS_250 = __VLS_249({
    modelValue: (__VLS_ctx.form.isCertified),
    activeValue: (1),
    inactiveValue: (0),
}, ...__VLS_functionalComponentArgsRest(__VLS_249));
var __VLS_247;
const __VLS_252 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_253 = __VLS_asFunctionalComponent(__VLS_252, new __VLS_252({
    label: "头像照片",
    ...{ class: "sf-form-full" },
}));
const __VLS_254 = __VLS_253({
    label: "头像照片",
    ...{ class: "sf-form-full" },
}, ...__VLS_functionalComponentArgsRest(__VLS_253));
__VLS_255.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ style: {} },
});
const __VLS_256 = {}.ElUpload;
/** @type {[typeof __VLS_components.ElUpload, typeof __VLS_components.elUpload, typeof __VLS_components.ElUpload, typeof __VLS_components.elUpload, ]} */ ;
// @ts-ignore
const __VLS_257 = __VLS_asFunctionalComponent(__VLS_256, new __VLS_256({
    showFileList: (false),
    beforeUpload: ((file) => __VLS_ctx.uploadPersonFile(file, 'avatar')),
    accept: ".jpg,.jpeg,image/jpeg",
}));
const __VLS_258 = __VLS_257({
    showFileList: (false),
    beforeUpload: ((file) => __VLS_ctx.uploadPersonFile(file, 'avatar')),
    accept: ".jpg,.jpeg,image/jpeg",
}, ...__VLS_functionalComponentArgsRest(__VLS_257));
__VLS_259.slots.default;
const __VLS_260 = {}.ElButton;
/** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
// @ts-ignore
const __VLS_261 = __VLS_asFunctionalComponent(__VLS_260, new __VLS_260({}));
const __VLS_262 = __VLS_261({}, ...__VLS_functionalComponentArgsRest(__VLS_261));
__VLS_263.slots.default;
var __VLS_263;
var __VLS_259;
if (__VLS_ctx.form.avatarFileId) {
    const __VLS_264 = {}.ElLink;
    /** @type {[typeof __VLS_components.ElLink, typeof __VLS_components.elLink, typeof __VLS_components.ElLink, typeof __VLS_components.elLink, ]} */ ;
    // @ts-ignore
    const __VLS_265 = __VLS_asFunctionalComponent(__VLS_264, new __VLS_264({
        href: (__VLS_ctx.fileUrl(__VLS_ctx.form.avatarFileId)),
        target: "_blank",
    }));
    const __VLS_266 = __VLS_265({
        href: (__VLS_ctx.fileUrl(__VLS_ctx.form.avatarFileId)),
        target: "_blank",
    }, ...__VLS_functionalComponentArgsRest(__VLS_265));
    __VLS_267.slots.default;
    var __VLS_267;
}
else {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({
        ...{ class: "sf-muted" },
    });
}
var __VLS_255;
const __VLS_268 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_269 = __VLS_asFunctionalComponent(__VLS_268, new __VLS_268({
    label: "证书附件",
    ...{ class: "sf-form-full" },
}));
const __VLS_270 = __VLS_269({
    label: "证书附件",
    ...{ class: "sf-form-full" },
}, ...__VLS_functionalComponentArgsRest(__VLS_269));
__VLS_271.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ style: {} },
});
const __VLS_272 = {}.ElUpload;
/** @type {[typeof __VLS_components.ElUpload, typeof __VLS_components.elUpload, typeof __VLS_components.ElUpload, typeof __VLS_components.elUpload, ]} */ ;
// @ts-ignore
const __VLS_273 = __VLS_asFunctionalComponent(__VLS_272, new __VLS_272({
    showFileList: (false),
    beforeUpload: ((file) => __VLS_ctx.uploadPersonFile(file, 'cert')),
    accept: ".jpg,.jpeg,image/jpeg",
}));
const __VLS_274 = __VLS_273({
    showFileList: (false),
    beforeUpload: ((file) => __VLS_ctx.uploadPersonFile(file, 'cert')),
    accept: ".jpg,.jpeg,image/jpeg",
}, ...__VLS_functionalComponentArgsRest(__VLS_273));
__VLS_275.slots.default;
const __VLS_276 = {}.ElButton;
/** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
// @ts-ignore
const __VLS_277 = __VLS_asFunctionalComponent(__VLS_276, new __VLS_276({}));
const __VLS_278 = __VLS_277({}, ...__VLS_functionalComponentArgsRest(__VLS_277));
__VLS_279.slots.default;
var __VLS_279;
var __VLS_275;
if (__VLS_ctx.form.certFileId) {
    const __VLS_280 = {}.ElLink;
    /** @type {[typeof __VLS_components.ElLink, typeof __VLS_components.elLink, typeof __VLS_components.ElLink, typeof __VLS_components.elLink, ]} */ ;
    // @ts-ignore
    const __VLS_281 = __VLS_asFunctionalComponent(__VLS_280, new __VLS_280({
        href: (__VLS_ctx.fileUrl(__VLS_ctx.form.certFileId)),
        target: "_blank",
    }));
    const __VLS_282 = __VLS_281({
        href: (__VLS_ctx.fileUrl(__VLS_ctx.form.certFileId)),
        target: "_blank",
    }, ...__VLS_functionalComponentArgsRest(__VLS_281));
    __VLS_283.slots.default;
    var __VLS_283;
}
else {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({
        ...{ class: "sf-muted" },
    });
}
var __VLS_271;
const __VLS_284 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_285 = __VLS_asFunctionalComponent(__VLS_284, new __VLS_284({
    label: "证书到期",
}));
const __VLS_286 = __VLS_285({
    label: "证书到期",
}, ...__VLS_functionalComponentArgsRest(__VLS_285));
__VLS_287.slots.default;
const __VLS_288 = {}.ElDatePicker;
/** @type {[typeof __VLS_components.ElDatePicker, typeof __VLS_components.elDatePicker, ]} */ ;
// @ts-ignore
const __VLS_289 = __VLS_asFunctionalComponent(__VLS_288, new __VLS_288({
    modelValue: (__VLS_ctx.form.certExpireDate),
    type: "date",
    valueFormat: "YYYY-MM-DD",
    placeholder: "选择日期",
    ...{ style: {} },
}));
const __VLS_290 = __VLS_289({
    modelValue: (__VLS_ctx.form.certExpireDate),
    type: "date",
    valueFormat: "YYYY-MM-DD",
    placeholder: "选择日期",
    ...{ style: {} },
}, ...__VLS_functionalComponentArgsRest(__VLS_289));
var __VLS_287;
const __VLS_292 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_293 = __VLS_asFunctionalComponent(__VLS_292, new __VLS_292({
    label: "短信推送",
}));
const __VLS_294 = __VLS_293({
    label: "短信推送",
}, ...__VLS_functionalComponentArgsRest(__VLS_293));
__VLS_295.slots.default;
const __VLS_296 = {}.ElSwitch;
/** @type {[typeof __VLS_components.ElSwitch, typeof __VLS_components.elSwitch, ]} */ ;
// @ts-ignore
const __VLS_297 = __VLS_asFunctionalComponent(__VLS_296, new __VLS_296({
    modelValue: (__VLS_ctx.form.smsNotify),
    activeValue: (1),
    inactiveValue: (0),
}));
const __VLS_298 = __VLS_297({
    modelValue: (__VLS_ctx.form.smsNotify),
    activeValue: (1),
    inactiveValue: (0),
}, ...__VLS_functionalComponentArgsRest(__VLS_297));
var __VLS_295;
const __VLS_300 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_301 = __VLS_asFunctionalComponent(__VLS_300, new __VLS_300({
    label: "电话",
}));
const __VLS_302 = __VLS_301({
    label: "电话",
}, ...__VLS_functionalComponentArgsRest(__VLS_301));
__VLS_303.slots.default;
const __VLS_304 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_305 = __VLS_asFunctionalComponent(__VLS_304, new __VLS_304({
    modelValue: (__VLS_ctx.form.phone),
}));
const __VLS_306 = __VLS_305({
    modelValue: (__VLS_ctx.form.phone),
}, ...__VLS_functionalComponentArgsRest(__VLS_305));
var __VLS_303;
var __VLS_211;
{
    const { footer: __VLS_thisSlot } = __VLS_207.slots;
    const __VLS_308 = {}.ElButton;
    /** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
    // @ts-ignore
    const __VLS_309 = __VLS_asFunctionalComponent(__VLS_308, new __VLS_308({
        ...{ 'onClick': {} },
    }));
    const __VLS_310 = __VLS_309({
        ...{ 'onClick': {} },
    }, ...__VLS_functionalComponentArgsRest(__VLS_309));
    let __VLS_312;
    let __VLS_313;
    let __VLS_314;
    const __VLS_315 = {
        onClick: (...[$event]) => {
            __VLS_ctx.dialogVisible = false;
        }
    };
    __VLS_311.slots.default;
    var __VLS_311;
    const __VLS_316 = {}.ElButton;
    /** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
    // @ts-ignore
    const __VLS_317 = __VLS_asFunctionalComponent(__VLS_316, new __VLS_316({
        ...{ 'onClick': {} },
        type: "primary",
        loading: (__VLS_ctx.saving),
    }));
    const __VLS_318 = __VLS_317({
        ...{ 'onClick': {} },
        type: "primary",
        loading: (__VLS_ctx.saving),
    }, ...__VLS_functionalComponentArgsRest(__VLS_317));
    let __VLS_320;
    let __VLS_321;
    let __VLS_322;
    const __VLS_323 = {
        onClick: (__VLS_ctx.onSave)
    };
    __VLS_319.slots.default;
    var __VLS_319;
}
var __VLS_207;
const __VLS_324 = {}.ElDialog;
/** @type {[typeof __VLS_components.ElDialog, typeof __VLS_components.elDialog, typeof __VLS_components.ElDialog, typeof __VLS_components.elDialog, ]} */ ;
// @ts-ignore
const __VLS_325 = __VLS_asFunctionalComponent(__VLS_324, new __VLS_324({
    modelValue: (__VLS_ctx.recordDialogVisible),
    title: (__VLS_ctx.recordEditing?.id ? '编辑出入记录' : '新增出入记录'),
    width: "680px",
}));
const __VLS_326 = __VLS_325({
    modelValue: (__VLS_ctx.recordDialogVisible),
    title: (__VLS_ctx.recordEditing?.id ? '编辑出入记录' : '新增出入记录'),
    width: "680px",
}, ...__VLS_functionalComponentArgsRest(__VLS_325));
__VLS_327.slots.default;
const __VLS_328 = {}.ElForm;
/** @type {[typeof __VLS_components.ElForm, typeof __VLS_components.elForm, typeof __VLS_components.ElForm, typeof __VLS_components.elForm, ]} */ ;
// @ts-ignore
const __VLS_329 = __VLS_asFunctionalComponent(__VLS_328, new __VLS_328({
    labelWidth: "110px",
}));
const __VLS_330 = __VLS_329({
    labelWidth: "110px",
}, ...__VLS_functionalComponentArgsRest(__VLS_329));
__VLS_331.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-form-grid" },
});
const __VLS_332 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_333 = __VLS_asFunctionalComponent(__VLS_332, new __VLS_332({
    label: "企业编码",
}));
const __VLS_334 = __VLS_333({
    label: "企业编码",
}, ...__VLS_functionalComponentArgsRest(__VLS_333));
__VLS_335.slots.default;
const __VLS_336 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_337 = __VLS_asFunctionalComponent(__VLS_336, new __VLS_336({
    modelValue: (__VLS_ctx.recordForm.companyCode),
    placeholder: "可选",
}));
const __VLS_338 = __VLS_337({
    modelValue: (__VLS_ctx.recordForm.companyCode),
    placeholder: "可选",
}, ...__VLS_functionalComponentArgsRest(__VLS_337));
var __VLS_335;
const __VLS_340 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_341 = __VLS_asFunctionalComponent(__VLS_340, new __VLS_340({
    label: "姓名",
    required: true,
}));
const __VLS_342 = __VLS_341({
    label: "姓名",
    required: true,
}, ...__VLS_functionalComponentArgsRest(__VLS_341));
__VLS_343.slots.default;
const __VLS_344 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_345 = __VLS_asFunctionalComponent(__VLS_344, new __VLS_344({
    modelValue: (__VLS_ctx.recordForm.personName),
}));
const __VLS_346 = __VLS_345({
    modelValue: (__VLS_ctx.recordForm.personName),
}, ...__VLS_functionalComponentArgsRest(__VLS_345));
var __VLS_343;
const __VLS_348 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_349 = __VLS_asFunctionalComponent(__VLS_348, new __VLS_348({
    label: "身份证号码",
    required: true,
}));
const __VLS_350 = __VLS_349({
    label: "身份证号码",
    required: true,
}, ...__VLS_functionalComponentArgsRest(__VLS_349));
__VLS_351.slots.default;
const __VLS_352 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_353 = __VLS_asFunctionalComponent(__VLS_352, new __VLS_352({
    modelValue: (__VLS_ctx.recordForm.idcard),
}));
const __VLS_354 = __VLS_353({
    modelValue: (__VLS_ctx.recordForm.idcard),
}, ...__VLS_functionalComponentArgsRest(__VLS_353));
var __VLS_351;
const __VLS_356 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_357 = __VLS_asFunctionalComponent(__VLS_356, new __VLS_356({
    label: "类型",
    required: true,
}));
const __VLS_358 = __VLS_357({
    label: "类型",
    required: true,
}, ...__VLS_functionalComponentArgsRest(__VLS_357));
__VLS_359.slots.default;
const __VLS_360 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_361 = __VLS_asFunctionalComponent(__VLS_360, new __VLS_360({
    modelValue: (__VLS_ctx.recordForm.personType),
}));
const __VLS_362 = __VLS_361({
    modelValue: (__VLS_ctx.recordForm.personType),
}, ...__VLS_functionalComponentArgsRest(__VLS_361));
var __VLS_359;
const __VLS_364 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_365 = __VLS_asFunctionalComponent(__VLS_364, new __VLS_364({
    label: "进出状态",
    required: true,
}));
const __VLS_366 = __VLS_365({
    label: "进出状态",
    required: true,
}, ...__VLS_functionalComponentArgsRest(__VLS_365));
__VLS_367.slots.default;
const __VLS_368 = {}.ElSelect;
/** @type {[typeof __VLS_components.ElSelect, typeof __VLS_components.elSelect, typeof __VLS_components.ElSelect, typeof __VLS_components.elSelect, ]} */ ;
// @ts-ignore
const __VLS_369 = __VLS_asFunctionalComponent(__VLS_368, new __VLS_368({
    modelValue: (__VLS_ctx.recordForm.inOutState),
    ...{ style: {} },
}));
const __VLS_370 = __VLS_369({
    modelValue: (__VLS_ctx.recordForm.inOutState),
    ...{ style: {} },
}, ...__VLS_functionalComponentArgsRest(__VLS_369));
__VLS_371.slots.default;
const __VLS_372 = {}.ElOption;
/** @type {[typeof __VLS_components.ElOption, typeof __VLS_components.elOption, ]} */ ;
// @ts-ignore
const __VLS_373 = __VLS_asFunctionalComponent(__VLS_372, new __VLS_372({
    label: "IN",
    value: "IN",
}));
const __VLS_374 = __VLS_373({
    label: "IN",
    value: "IN",
}, ...__VLS_functionalComponentArgsRest(__VLS_373));
const __VLS_376 = {}.ElOption;
/** @type {[typeof __VLS_components.ElOption, typeof __VLS_components.elOption, ]} */ ;
// @ts-ignore
const __VLS_377 = __VLS_asFunctionalComponent(__VLS_376, new __VLS_376({
    label: "OUT",
    value: "OUT",
}));
const __VLS_378 = __VLS_377({
    label: "OUT",
    value: "OUT",
}, ...__VLS_functionalComponentArgsRest(__VLS_377));
var __VLS_371;
var __VLS_367;
const __VLS_380 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_381 = __VLS_asFunctionalComponent(__VLS_380, new __VLS_380({
    label: "进出时间",
    required: true,
}));
const __VLS_382 = __VLS_381({
    label: "进出时间",
    required: true,
}, ...__VLS_functionalComponentArgsRest(__VLS_381));
__VLS_383.slots.default;
const __VLS_384 = {}.ElDatePicker;
/** @type {[typeof __VLS_components.ElDatePicker, typeof __VLS_components.elDatePicker, ]} */ ;
// @ts-ignore
const __VLS_385 = __VLS_asFunctionalComponent(__VLS_384, new __VLS_384({
    modelValue: (__VLS_ctx.recordForm.inOutTime),
    type: "datetime",
    valueFormat: "x",
    placeholder: "选择时间",
    ...{ style: {} },
}));
const __VLS_386 = __VLS_385({
    modelValue: (__VLS_ctx.recordForm.inOutTime),
    type: "datetime",
    valueFormat: "x",
    placeholder: "选择时间",
    ...{ style: {} },
}, ...__VLS_functionalComponentArgsRest(__VLS_385));
var __VLS_383;
const __VLS_388 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_389 = __VLS_asFunctionalComponent(__VLS_388, new __VLS_388({
    label: "图片",
    ...{ class: "sf-form-full" },
}));
const __VLS_390 = __VLS_389({
    label: "图片",
    ...{ class: "sf-form-full" },
}, ...__VLS_functionalComponentArgsRest(__VLS_389));
__VLS_391.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ style: {} },
});
const __VLS_392 = {}.ElUpload;
/** @type {[typeof __VLS_components.ElUpload, typeof __VLS_components.elUpload, typeof __VLS_components.ElUpload, typeof __VLS_components.elUpload, ]} */ ;
// @ts-ignore
const __VLS_393 = __VLS_asFunctionalComponent(__VLS_392, new __VLS_392({
    showFileList: (false),
    beforeUpload: (__VLS_ctx.uploadRecordImage),
    accept: ".jpg,.jpeg,image/jpeg",
}));
const __VLS_394 = __VLS_393({
    showFileList: (false),
    beforeUpload: (__VLS_ctx.uploadRecordImage),
    accept: ".jpg,.jpeg,image/jpeg",
}, ...__VLS_functionalComponentArgsRest(__VLS_393));
__VLS_395.slots.default;
const __VLS_396 = {}.ElButton;
/** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
// @ts-ignore
const __VLS_397 = __VLS_asFunctionalComponent(__VLS_396, new __VLS_396({}));
const __VLS_398 = __VLS_397({}, ...__VLS_functionalComponentArgsRest(__VLS_397));
__VLS_399.slots.default;
var __VLS_399;
var __VLS_395;
if (__VLS_ctx.recordForm.imageFileId) {
    const __VLS_400 = {}.ElLink;
    /** @type {[typeof __VLS_components.ElLink, typeof __VLS_components.elLink, typeof __VLS_components.ElLink, typeof __VLS_components.elLink, ]} */ ;
    // @ts-ignore
    const __VLS_401 = __VLS_asFunctionalComponent(__VLS_400, new __VLS_400({
        href: (__VLS_ctx.fileUrl(__VLS_ctx.recordForm.imageFileId)),
        target: "_blank",
    }));
    const __VLS_402 = __VLS_401({
        href: (__VLS_ctx.fileUrl(__VLS_ctx.recordForm.imageFileId)),
        target: "_blank",
    }, ...__VLS_functionalComponentArgsRest(__VLS_401));
    __VLS_403.slots.default;
    var __VLS_403;
}
else {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({
        ...{ class: "sf-muted" },
    });
}
var __VLS_391;
var __VLS_331;
{
    const { footer: __VLS_thisSlot } = __VLS_327.slots;
    const __VLS_404 = {}.ElButton;
    /** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
    // @ts-ignore
    const __VLS_405 = __VLS_asFunctionalComponent(__VLS_404, new __VLS_404({
        ...{ 'onClick': {} },
    }));
    const __VLS_406 = __VLS_405({
        ...{ 'onClick': {} },
    }, ...__VLS_functionalComponentArgsRest(__VLS_405));
    let __VLS_408;
    let __VLS_409;
    let __VLS_410;
    const __VLS_411 = {
        onClick: (...[$event]) => {
            __VLS_ctx.recordDialogVisible = false;
        }
    };
    __VLS_407.slots.default;
    var __VLS_407;
    const __VLS_412 = {}.ElButton;
    /** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
    // @ts-ignore
    const __VLS_413 = __VLS_asFunctionalComponent(__VLS_412, new __VLS_412({
        ...{ 'onClick': {} },
        type: "primary",
        loading: (__VLS_ctx.recordSaving),
    }));
    const __VLS_414 = __VLS_413({
        ...{ 'onClick': {} },
        type: "primary",
        loading: (__VLS_ctx.recordSaving),
    }, ...__VLS_functionalComponentArgsRest(__VLS_413));
    let __VLS_416;
    let __VLS_417;
    let __VLS_418;
    const __VLS_419 = {
        onClick: (__VLS_ctx.saveRecord)
    };
    __VLS_415.slots.default;
    var __VLS_415;
}
var __VLS_327;
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
/** @type {__VLS_StyleScopedClasses['sf-chip']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-filter']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-muted']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-form-grid']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-form-full']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-muted']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-form-full']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-muted']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-form-grid']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-form-full']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-muted']} */ ;
var __VLS_dollars;
const __VLS_self = (await import('vue')).defineComponent({
    setup() {
        return {
            fileUrl: fileUrl,
            tab: tab,
            keyword: keyword,
            loadingPeople: loadingPeople,
            people: people,
            peopleTotal: peopleTotal,
            peoplePage: peoplePage,
            peoplePageSize: peoplePageSize,
            loadingRecords: loadingRecords,
            records: records,
            recordsTotal: recordsTotal,
            recordsPage: recordsPage,
            recordsPageSize: recordsPageSize,
            recordDialogVisible: recordDialogVisible,
            recordSaving: recordSaving,
            recordEditing: recordEditing,
            recordForm: recordForm,
            dialogVisible: dialogVisible,
            saving: saving,
            editing: editing,
            form: form,
            fmt: fmt,
            loadPeople: loadPeople,
            loadRecords: loadRecords,
            openRecordCreate: openRecordCreate,
            openRecordEdit: openRecordEdit,
            uploadRecordImage: uploadRecordImage,
            saveRecord: saveRecord,
            deleteRecord: deleteRecord,
            importPeople: importPeople,
            openCreate: openCreate,
            openEdit: openEdit,
            uploadPersonFile: uploadPersonFile,
            onSave: onSave,
            onDelete: onDelete,
        };
    },
});
export default (await import('vue')).defineComponent({
    setup() {
        return {};
    },
});
; /* PartiallyEnd: #4569/main.vue */
