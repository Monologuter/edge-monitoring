import { reactive, ref, watch } from "vue";
import { ElMessage, ElMessageBox } from "element-plus";
import { fileUrl, http, rawHttp, uploadFile } from "@/api/http";
const tab = ref("cars");
const keyword = ref("");
const loadingCars = ref(false);
const cars = ref([]);
const carsTotal = ref(0);
const carsPage = ref(1);
const carsPageSize = ref(20);
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
    licensePlateNumber: "",
    carType: "",
    inOutState: "IN",
    inOutTime: "",
    imageFileId: null
});
const dialogVisible = ref(false);
const saving = ref(false);
const editing = ref(null);
const form = reactive({
    companyCode: "",
    licensePlateNumber: "",
    driverName: "",
    driverPhone: "",
    validStart: null,
    validEnd: null,
    licenseFileId: null,
    carType: ""
});
function fmt(ts) {
    const d = new Date(ts);
    const pad = (n) => String(n).padStart(2, "0");
    return `${d.getFullYear()}-${pad(d.getMonth() + 1)}-${pad(d.getDate())} ${pad(d.getHours())}:${pad(d.getMinutes())}:${pad(d.getSeconds())}`;
}
async function loadCars() {
    loadingCars.value = true;
    try {
        const data = await http.get(`/api/v1/cars?page=${carsPage.value}&pageSize=${carsPageSize.value}${keyword.value ? `&keyword=${encodeURIComponent(keyword.value)}` : ""}`);
        cars.value = data.list;
        carsTotal.value = data.total;
    }
    catch (e) {
        ElMessage.error(e?.message || "加载失败");
    }
    finally {
        loadingCars.value = false;
    }
}
async function loadRecords() {
    loadingRecords.value = true;
    try {
        const data = await http.get(`/api/v1/car-inout?page=${recordsPage.value}&pageSize=${recordsPageSize.value}`);
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
        licensePlateNumber: "",
        carType: "",
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
        licensePlateNumber: row.licensePlateNumber,
        carType: row.carType || "",
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
        const out = await uploadFile("car_inout", file);
        recordForm.imageFileId = out.id;
        ElMessage.success("上传成功");
    }
    catch (e) {
        ElMessage.error(e?.message || "上传失败");
    }
    return false;
}
async function saveRecord() {
    if (!recordForm.licensePlateNumber || !recordForm.inOutTime) {
        ElMessage.warning("请补全必填项");
        return;
    }
    recordSaving.value = true;
    try {
        const payload = {
            id: recordEditing.value?.id,
            companyCode: recordForm.companyCode || null,
            licensePlateNumber: recordForm.licensePlateNumber,
            carType: recordForm.carType || null,
            inOutState: recordForm.inOutState,
            inOutTime: Number(recordForm.inOutTime),
            imageFileId: recordForm.imageFileId
        };
        if (recordEditing.value?.id) {
            await http.put("/api/v1/car-inout", payload);
        }
        else {
            await http.post("/api/v1/car-inout", payload);
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
        await http.del(`/api/v1/car-inout/${id}`);
        ElMessage.success("删除成功");
        await loadRecords();
    }
    catch (e) {
        ElMessage.error(e?.message || "删除失败");
    }
}
async function importCars(file) {
    const fd = new FormData();
    fd.append("file", file);
    try {
        const res = await rawHttp.post("/api/v1/cars/import", fd, {
            headers: { "Content-Type": "multipart/form-data" }
        });
        ElMessage.success(`导入完成：成功 ${res.data.successCount} 条，失败 ${res.data.failCount} 条`);
        await loadCars();
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
        licensePlateNumber: "",
        driverName: "",
        driverPhone: "",
        validStart: null,
        validEnd: null,
        licenseFileId: null,
        carType: ""
    });
    dialogVisible.value = true;
}
function openEdit(row) {
    editing.value = row;
    Object.assign(form, {
        companyCode: row.companyCode,
        licensePlateNumber: row.licensePlateNumber,
        driverName: row.driverName || "",
        driverPhone: row.driverPhone || "",
        validStart: row.validStart || null,
        validEnd: row.validEnd || null,
        licenseFileId: row.licenseFileId || null,
        carType: row.carType || ""
    });
    dialogVisible.value = true;
}
function isJpg(file) {
    const name = file.name.toLowerCase();
    return file.type === "image/jpeg" || name.endsWith(".jpg") || name.endsWith(".jpeg");
}
async function uploadLicenseFile(file) {
    if (!isJpg(file)) {
        ElMessage.warning("请上传 JPG 图片");
        return false;
    }
    try {
        const out = await uploadFile("car_license", file);
        form.licenseFileId = out.id;
        ElMessage.success("上传成功");
    }
    catch (e) {
        ElMessage.error(e?.message || "上传失败");
    }
    return false;
}
async function onSave() {
    if (!form.companyCode || !form.licensePlateNumber) {
        ElMessage.warning("请填写企业编码与车牌号");
        return;
    }
    saving.value = true;
    try {
        if (!editing.value?.id) {
            await http.post("/api/v1/cars", { ...form });
        }
        else {
            await http.put("/api/v1/cars", { id: editing.value.id, ...form });
        }
        ElMessage.success("保存成功");
        dialogVisible.value = false;
        await loadCars();
    }
    catch (e) {
        ElMessage.error(e?.message || "保存失败");
    }
    finally {
        saving.value = false;
    }
}
async function onDelete(id) {
    const ok = await ElMessageBox.confirm("确认删除该车辆？", "提示", { type: "warning" }).catch(() => null);
    if (!ok)
        return;
    try {
        await http.del(`/api/v1/cars/${id}`);
        ElMessage.success("删除成功");
        await loadCars();
    }
    catch (e) {
        ElMessage.error(e?.message || "删除失败");
    }
}
watch([carsPage, carsPageSize], () => loadCars());
watch([recordsPage, recordsPageSize], () => loadRecords());
watch(keyword, () => {
    carsPage.value = 1;
    loadCars();
});
watch(tab, () => {
    if (tab.value === "records")
        loadRecords();
});
loadCars();
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
    placeholder: "车牌/企业编码",
    ...{ style: {} },
    clearable: true,
}));
const __VLS_2 = __VLS_1({
    modelValue: (__VLS_ctx.keyword),
    placeholder: "车牌/企业编码",
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
    beforeUpload: (__VLS_ctx.importCars),
    accept: ".csv",
}));
const __VLS_14 = __VLS_13({
    showFileList: (false),
    beforeUpload: (__VLS_ctx.importCars),
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
    onClick: (__VLS_ctx.loadCars)
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
    label: "车辆档案",
    name: "cars",
}));
const __VLS_34 = __VLS_33({
    label: "车辆档案",
    name: "cars",
}, ...__VLS_functionalComponentArgsRest(__VLS_33));
__VLS_35.slots.default;
const __VLS_36 = {}.ElTable;
/** @type {[typeof __VLS_components.ElTable, typeof __VLS_components.elTable, typeof __VLS_components.ElTable, typeof __VLS_components.elTable, ]} */ ;
// @ts-ignore
const __VLS_37 = __VLS_asFunctionalComponent(__VLS_36, new __VLS_36({
    data: (__VLS_ctx.cars),
    ...{ style: {} },
}));
const __VLS_38 = __VLS_37({
    data: (__VLS_ctx.cars),
    ...{ style: {} },
}, ...__VLS_functionalComponentArgsRest(__VLS_37));
__VLS_asFunctionalDirective(__VLS_directives.vLoading)(null, { ...__VLS_directiveBindingRestFields, value: (__VLS_ctx.loadingCars) }, null, null);
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
    prop: "licensePlateNumber",
    label: "车牌号",
    minWidth: "140",
}));
const __VLS_46 = __VLS_45({
    prop: "licensePlateNumber",
    label: "车牌号",
    minWidth: "140",
}, ...__VLS_functionalComponentArgsRest(__VLS_45));
const __VLS_48 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_49 = __VLS_asFunctionalComponent(__VLS_48, new __VLS_48({
    prop: "driverName",
    label: "司机姓名",
    width: "120",
}));
const __VLS_50 = __VLS_49({
    prop: "driverName",
    label: "司机姓名",
    width: "120",
}, ...__VLS_functionalComponentArgsRest(__VLS_49));
const __VLS_52 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_53 = __VLS_asFunctionalComponent(__VLS_52, new __VLS_52({
    prop: "driverPhone",
    label: "司机手机号",
    width: "140",
}));
const __VLS_54 = __VLS_53({
    prop: "driverPhone",
    label: "司机手机号",
    width: "140",
}, ...__VLS_functionalComponentArgsRest(__VLS_53));
const __VLS_56 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_57 = __VLS_asFunctionalComponent(__VLS_56, new __VLS_56({
    prop: "validStart",
    label: "有效开始",
    width: "130",
}));
const __VLS_58 = __VLS_57({
    prop: "validStart",
    label: "有效开始",
    width: "130",
}, ...__VLS_functionalComponentArgsRest(__VLS_57));
__VLS_59.slots.default;
{
    const { default: __VLS_thisSlot } = __VLS_59.slots;
    const [{ row }] = __VLS_getSlotParams(__VLS_thisSlot);
    (row.validStart || "-");
}
var __VLS_59;
const __VLS_60 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_61 = __VLS_asFunctionalComponent(__VLS_60, new __VLS_60({
    prop: "validEnd",
    label: "有效结束",
    width: "130",
}));
const __VLS_62 = __VLS_61({
    prop: "validEnd",
    label: "有效结束",
    width: "130",
}, ...__VLS_functionalComponentArgsRest(__VLS_61));
__VLS_63.slots.default;
{
    const { default: __VLS_thisSlot } = __VLS_63.slots;
    const [{ row }] = __VLS_getSlotParams(__VLS_thisSlot);
    (row.validEnd || "-");
}
var __VLS_63;
const __VLS_64 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_65 = __VLS_asFunctionalComponent(__VLS_64, new __VLS_64({
    label: "行驶证附件",
    width: "110",
}));
const __VLS_66 = __VLS_65({
    label: "行驶证附件",
    width: "110",
}, ...__VLS_functionalComponentArgsRest(__VLS_65));
__VLS_67.slots.default;
{
    const { default: __VLS_thisSlot } = __VLS_67.slots;
    const [{ row }] = __VLS_getSlotParams(__VLS_thisSlot);
    if (row.licenseFileId) {
        const __VLS_68 = {}.ElLink;
        /** @type {[typeof __VLS_components.ElLink, typeof __VLS_components.elLink, typeof __VLS_components.ElLink, typeof __VLS_components.elLink, ]} */ ;
        // @ts-ignore
        const __VLS_69 = __VLS_asFunctionalComponent(__VLS_68, new __VLS_68({
            href: (__VLS_ctx.fileUrl(row.licenseFileId)),
            target: "_blank",
        }));
        const __VLS_70 = __VLS_69({
            href: (__VLS_ctx.fileUrl(row.licenseFileId)),
            target: "_blank",
        }, ...__VLS_functionalComponentArgsRest(__VLS_69));
        __VLS_71.slots.default;
        var __VLS_71;
    }
    else {
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({
            ...{ class: "sf-muted" },
        });
    }
}
var __VLS_67;
const __VLS_72 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_73 = __VLS_asFunctionalComponent(__VLS_72, new __VLS_72({
    prop: "carType",
    label: "车辆类型",
    width: "140",
}));
const __VLS_74 = __VLS_73({
    prop: "carType",
    label: "车辆类型",
    width: "140",
}, ...__VLS_functionalComponentArgsRest(__VLS_73));
const __VLS_76 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_77 = __VLS_asFunctionalComponent(__VLS_76, new __VLS_76({
    prop: "dataSyncTime",
    label: "同步时间",
    minWidth: "170",
}));
const __VLS_78 = __VLS_77({
    prop: "dataSyncTime",
    label: "同步时间",
    minWidth: "170",
}, ...__VLS_functionalComponentArgsRest(__VLS_77));
__VLS_79.slots.default;
{
    const { default: __VLS_thisSlot } = __VLS_79.slots;
    const [{ row }] = __VLS_getSlotParams(__VLS_thisSlot);
    (row.dataSyncTime ? __VLS_ctx.fmt(row.dataSyncTime) : "-");
}
var __VLS_79;
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
var __VLS_39;
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
    total: (__VLS_ctx.carsTotal),
    currentPage: (__VLS_ctx.carsPage),
    pageSize: (__VLS_ctx.carsPageSize),
}));
const __VLS_102 = __VLS_101({
    ...{ 'onChange': {} },
    background: true,
    layout: "prev, pager, next, sizes, total",
    total: (__VLS_ctx.carsTotal),
    currentPage: (__VLS_ctx.carsPage),
    pageSize: (__VLS_ctx.carsPageSize),
}, ...__VLS_functionalComponentArgsRest(__VLS_101));
let __VLS_104;
let __VLS_105;
let __VLS_106;
const __VLS_107 = {
    onChange: (__VLS_ctx.loadCars)
};
var __VLS_103;
var __VLS_35;
const __VLS_108 = {}.ElTabPane;
/** @type {[typeof __VLS_components.ElTabPane, typeof __VLS_components.elTabPane, typeof __VLS_components.ElTabPane, typeof __VLS_components.elTabPane, ]} */ ;
// @ts-ignore
const __VLS_109 = __VLS_asFunctionalComponent(__VLS_108, new __VLS_108({
    label: "出入记录",
    name: "records",
}));
const __VLS_110 = __VLS_109({
    label: "出入记录",
    name: "records",
}, ...__VLS_functionalComponentArgsRest(__VLS_109));
__VLS_111.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-filter" },
    ...{ style: {} },
});
const __VLS_112 = {}.ElButton;
/** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
// @ts-ignore
const __VLS_113 = __VLS_asFunctionalComponent(__VLS_112, new __VLS_112({
    ...{ 'onClick': {} },
    type: "primary",
}));
const __VLS_114 = __VLS_113({
    ...{ 'onClick': {} },
    type: "primary",
}, ...__VLS_functionalComponentArgsRest(__VLS_113));
let __VLS_116;
let __VLS_117;
let __VLS_118;
const __VLS_119 = {
    onClick: (__VLS_ctx.openRecordCreate)
};
__VLS_115.slots.default;
var __VLS_115;
const __VLS_120 = {}.ElButton;
/** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
// @ts-ignore
const __VLS_121 = __VLS_asFunctionalComponent(__VLS_120, new __VLS_120({
    ...{ 'onClick': {} },
}));
const __VLS_122 = __VLS_121({
    ...{ 'onClick': {} },
}, ...__VLS_functionalComponentArgsRest(__VLS_121));
let __VLS_124;
let __VLS_125;
let __VLS_126;
const __VLS_127 = {
    onClick: (__VLS_ctx.loadRecords)
};
__VLS_123.slots.default;
var __VLS_123;
const __VLS_128 = {}.ElTable;
/** @type {[typeof __VLS_components.ElTable, typeof __VLS_components.elTable, typeof __VLS_components.ElTable, typeof __VLS_components.elTable, ]} */ ;
// @ts-ignore
const __VLS_129 = __VLS_asFunctionalComponent(__VLS_128, new __VLS_128({
    data: (__VLS_ctx.records),
    ...{ style: {} },
}));
const __VLS_130 = __VLS_129({
    data: (__VLS_ctx.records),
    ...{ style: {} },
}, ...__VLS_functionalComponentArgsRest(__VLS_129));
__VLS_asFunctionalDirective(__VLS_directives.vLoading)(null, { ...__VLS_directiveBindingRestFields, value: (__VLS_ctx.loadingRecords) }, null, null);
__VLS_131.slots.default;
const __VLS_132 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_133 = __VLS_asFunctionalComponent(__VLS_132, new __VLS_132({
    prop: "companyCode",
    label: "企业编码",
    minWidth: "140",
}));
const __VLS_134 = __VLS_133({
    prop: "companyCode",
    label: "企业编码",
    minWidth: "140",
}, ...__VLS_functionalComponentArgsRest(__VLS_133));
const __VLS_136 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_137 = __VLS_asFunctionalComponent(__VLS_136, new __VLS_136({
    prop: "licensePlateNumber",
    label: "车牌号",
    minWidth: "140",
}));
const __VLS_138 = __VLS_137({
    prop: "licensePlateNumber",
    label: "车牌号",
    minWidth: "140",
}, ...__VLS_functionalComponentArgsRest(__VLS_137));
const __VLS_140 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_141 = __VLS_asFunctionalComponent(__VLS_140, new __VLS_140({
    prop: "driverName",
    label: "司机姓名",
    width: "120",
}));
const __VLS_142 = __VLS_141({
    prop: "driverName",
    label: "司机姓名",
    width: "120",
}, ...__VLS_functionalComponentArgsRest(__VLS_141));
__VLS_143.slots.default;
{
    const { default: __VLS_thisSlot } = __VLS_143.slots;
    const [{ row }] = __VLS_getSlotParams(__VLS_thisSlot);
    (row.driverName || "-");
}
var __VLS_143;
const __VLS_144 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_145 = __VLS_asFunctionalComponent(__VLS_144, new __VLS_144({
    prop: "carType",
    label: "车辆类型",
    width: "140",
}));
const __VLS_146 = __VLS_145({
    prop: "carType",
    label: "车辆类型",
    width: "140",
}, ...__VLS_functionalComponentArgsRest(__VLS_145));
const __VLS_148 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_149 = __VLS_asFunctionalComponent(__VLS_148, new __VLS_148({
    prop: "inOutState",
    label: "出入状态",
    width: "110",
}));
const __VLS_150 = __VLS_149({
    prop: "inOutState",
    label: "出入状态",
    width: "110",
}, ...__VLS_functionalComponentArgsRest(__VLS_149));
__VLS_151.slots.default;
{
    const { default: __VLS_thisSlot } = __VLS_151.slots;
    const [{ row }] = __VLS_getSlotParams(__VLS_thisSlot);
    (row.inOutState === "IN" ? "进" : row.inOutState === "OUT" ? "出" : row.inOutState);
}
var __VLS_151;
const __VLS_152 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_153 = __VLS_asFunctionalComponent(__VLS_152, new __VLS_152({
    prop: "inOutTime",
    label: "进出时间",
    minWidth: "180",
}));
const __VLS_154 = __VLS_153({
    prop: "inOutTime",
    label: "进出时间",
    minWidth: "180",
}, ...__VLS_functionalComponentArgsRest(__VLS_153));
__VLS_155.slots.default;
{
    const { default: __VLS_thisSlot } = __VLS_155.slots;
    const [{ row }] = __VLS_getSlotParams(__VLS_thisSlot);
    (__VLS_ctx.fmt(row.inOutTime));
}
var __VLS_155;
const __VLS_156 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_157 = __VLS_asFunctionalComponent(__VLS_156, new __VLS_156({
    label: "车辆照片",
    width: "110",
}));
const __VLS_158 = __VLS_157({
    label: "车辆照片",
    width: "110",
}, ...__VLS_functionalComponentArgsRest(__VLS_157));
__VLS_159.slots.default;
{
    const { default: __VLS_thisSlot } = __VLS_159.slots;
    const [{ row }] = __VLS_getSlotParams(__VLS_thisSlot);
    if (row.imageFileId) {
        const __VLS_160 = {}.ElLink;
        /** @type {[typeof __VLS_components.ElLink, typeof __VLS_components.elLink, typeof __VLS_components.ElLink, typeof __VLS_components.elLink, ]} */ ;
        // @ts-ignore
        const __VLS_161 = __VLS_asFunctionalComponent(__VLS_160, new __VLS_160({
            href: (__VLS_ctx.fileUrl(row.imageFileId)),
            target: "_blank",
        }));
        const __VLS_162 = __VLS_161({
            href: (__VLS_ctx.fileUrl(row.imageFileId)),
            target: "_blank",
        }, ...__VLS_functionalComponentArgsRest(__VLS_161));
        __VLS_163.slots.default;
        var __VLS_163;
    }
    else {
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({
            ...{ class: "sf-muted" },
        });
    }
}
var __VLS_159;
const __VLS_164 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_165 = __VLS_asFunctionalComponent(__VLS_164, new __VLS_164({
    label: "操作",
    width: "160",
    fixed: "right",
}));
const __VLS_166 = __VLS_165({
    label: "操作",
    width: "160",
    fixed: "right",
}, ...__VLS_functionalComponentArgsRest(__VLS_165));
__VLS_167.slots.default;
{
    const { default: __VLS_thisSlot } = __VLS_167.slots;
    const [{ row }] = __VLS_getSlotParams(__VLS_thisSlot);
    const __VLS_168 = {}.ElButton;
    /** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
    // @ts-ignore
    const __VLS_169 = __VLS_asFunctionalComponent(__VLS_168, new __VLS_168({
        ...{ 'onClick': {} },
        size: "small",
    }));
    const __VLS_170 = __VLS_169({
        ...{ 'onClick': {} },
        size: "small",
    }, ...__VLS_functionalComponentArgsRest(__VLS_169));
    let __VLS_172;
    let __VLS_173;
    let __VLS_174;
    const __VLS_175 = {
        onClick: (...[$event]) => {
            __VLS_ctx.openRecordEdit(row);
        }
    };
    __VLS_171.slots.default;
    var __VLS_171;
    const __VLS_176 = {}.ElButton;
    /** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
    // @ts-ignore
    const __VLS_177 = __VLS_asFunctionalComponent(__VLS_176, new __VLS_176({
        ...{ 'onClick': {} },
        size: "small",
        type: "danger",
        plain: true,
    }));
    const __VLS_178 = __VLS_177({
        ...{ 'onClick': {} },
        size: "small",
        type: "danger",
        plain: true,
    }, ...__VLS_functionalComponentArgsRest(__VLS_177));
    let __VLS_180;
    let __VLS_181;
    let __VLS_182;
    const __VLS_183 = {
        onClick: (...[$event]) => {
            __VLS_ctx.deleteRecord(row.id);
        }
    };
    __VLS_179.slots.default;
    var __VLS_179;
}
var __VLS_167;
var __VLS_131;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ style: {} },
});
const __VLS_184 = {}.ElPagination;
/** @type {[typeof __VLS_components.ElPagination, typeof __VLS_components.elPagination, ]} */ ;
// @ts-ignore
const __VLS_185 = __VLS_asFunctionalComponent(__VLS_184, new __VLS_184({
    ...{ 'onChange': {} },
    background: true,
    layout: "prev, pager, next, sizes, total",
    total: (__VLS_ctx.recordsTotal),
    currentPage: (__VLS_ctx.recordsPage),
    pageSize: (__VLS_ctx.recordsPageSize),
}));
const __VLS_186 = __VLS_185({
    ...{ 'onChange': {} },
    background: true,
    layout: "prev, pager, next, sizes, total",
    total: (__VLS_ctx.recordsTotal),
    currentPage: (__VLS_ctx.recordsPage),
    pageSize: (__VLS_ctx.recordsPageSize),
}, ...__VLS_functionalComponentArgsRest(__VLS_185));
let __VLS_188;
let __VLS_189;
let __VLS_190;
const __VLS_191 = {
    onChange: (__VLS_ctx.loadRecords)
};
var __VLS_187;
var __VLS_111;
var __VLS_31;
const __VLS_192 = {}.ElDialog;
/** @type {[typeof __VLS_components.ElDialog, typeof __VLS_components.elDialog, typeof __VLS_components.ElDialog, typeof __VLS_components.elDialog, ]} */ ;
// @ts-ignore
const __VLS_193 = __VLS_asFunctionalComponent(__VLS_192, new __VLS_192({
    modelValue: (__VLS_ctx.dialogVisible),
    title: (__VLS_ctx.editing?.id ? '编辑车辆' : '新增车辆'),
    width: "720px",
}));
const __VLS_194 = __VLS_193({
    modelValue: (__VLS_ctx.dialogVisible),
    title: (__VLS_ctx.editing?.id ? '编辑车辆' : '新增车辆'),
    width: "720px",
}, ...__VLS_functionalComponentArgsRest(__VLS_193));
__VLS_195.slots.default;
const __VLS_196 = {}.ElForm;
/** @type {[typeof __VLS_components.ElForm, typeof __VLS_components.elForm, typeof __VLS_components.ElForm, typeof __VLS_components.elForm, ]} */ ;
// @ts-ignore
const __VLS_197 = __VLS_asFunctionalComponent(__VLS_196, new __VLS_196({
    labelWidth: "110px",
}));
const __VLS_198 = __VLS_197({
    labelWidth: "110px",
}, ...__VLS_functionalComponentArgsRest(__VLS_197));
__VLS_199.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-form-grid" },
});
const __VLS_200 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_201 = __VLS_asFunctionalComponent(__VLS_200, new __VLS_200({
    label: "企业编码",
    required: true,
}));
const __VLS_202 = __VLS_201({
    label: "企业编码",
    required: true,
}, ...__VLS_functionalComponentArgsRest(__VLS_201));
__VLS_203.slots.default;
const __VLS_204 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_205 = __VLS_asFunctionalComponent(__VLS_204, new __VLS_204({
    modelValue: (__VLS_ctx.form.companyCode),
}));
const __VLS_206 = __VLS_205({
    modelValue: (__VLS_ctx.form.companyCode),
}, ...__VLS_functionalComponentArgsRest(__VLS_205));
var __VLS_203;
const __VLS_208 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_209 = __VLS_asFunctionalComponent(__VLS_208, new __VLS_208({
    label: "车牌号",
    required: true,
}));
const __VLS_210 = __VLS_209({
    label: "车牌号",
    required: true,
}, ...__VLS_functionalComponentArgsRest(__VLS_209));
__VLS_211.slots.default;
const __VLS_212 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_213 = __VLS_asFunctionalComponent(__VLS_212, new __VLS_212({
    modelValue: (__VLS_ctx.form.licensePlateNumber),
}));
const __VLS_214 = __VLS_213({
    modelValue: (__VLS_ctx.form.licensePlateNumber),
}, ...__VLS_functionalComponentArgsRest(__VLS_213));
var __VLS_211;
const __VLS_216 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_217 = __VLS_asFunctionalComponent(__VLS_216, new __VLS_216({
    label: "司机姓名",
}));
const __VLS_218 = __VLS_217({
    label: "司机姓名",
}, ...__VLS_functionalComponentArgsRest(__VLS_217));
__VLS_219.slots.default;
const __VLS_220 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_221 = __VLS_asFunctionalComponent(__VLS_220, new __VLS_220({
    modelValue: (__VLS_ctx.form.driverName),
}));
const __VLS_222 = __VLS_221({
    modelValue: (__VLS_ctx.form.driverName),
}, ...__VLS_functionalComponentArgsRest(__VLS_221));
var __VLS_219;
const __VLS_224 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_225 = __VLS_asFunctionalComponent(__VLS_224, new __VLS_224({
    label: "司机手机号",
}));
const __VLS_226 = __VLS_225({
    label: "司机手机号",
}, ...__VLS_functionalComponentArgsRest(__VLS_225));
__VLS_227.slots.default;
const __VLS_228 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_229 = __VLS_asFunctionalComponent(__VLS_228, new __VLS_228({
    modelValue: (__VLS_ctx.form.driverPhone),
}));
const __VLS_230 = __VLS_229({
    modelValue: (__VLS_ctx.form.driverPhone),
}, ...__VLS_functionalComponentArgsRest(__VLS_229));
var __VLS_227;
const __VLS_232 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_233 = __VLS_asFunctionalComponent(__VLS_232, new __VLS_232({
    label: "有效开始",
}));
const __VLS_234 = __VLS_233({
    label: "有效开始",
}, ...__VLS_functionalComponentArgsRest(__VLS_233));
__VLS_235.slots.default;
const __VLS_236 = {}.ElDatePicker;
/** @type {[typeof __VLS_components.ElDatePicker, typeof __VLS_components.elDatePicker, ]} */ ;
// @ts-ignore
const __VLS_237 = __VLS_asFunctionalComponent(__VLS_236, new __VLS_236({
    modelValue: (__VLS_ctx.form.validStart),
    type: "date",
    valueFormat: "YYYY-MM-DD",
    placeholder: "选择日期",
    ...{ style: {} },
}));
const __VLS_238 = __VLS_237({
    modelValue: (__VLS_ctx.form.validStart),
    type: "date",
    valueFormat: "YYYY-MM-DD",
    placeholder: "选择日期",
    ...{ style: {} },
}, ...__VLS_functionalComponentArgsRest(__VLS_237));
var __VLS_235;
const __VLS_240 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_241 = __VLS_asFunctionalComponent(__VLS_240, new __VLS_240({
    label: "有效结束",
}));
const __VLS_242 = __VLS_241({
    label: "有效结束",
}, ...__VLS_functionalComponentArgsRest(__VLS_241));
__VLS_243.slots.default;
const __VLS_244 = {}.ElDatePicker;
/** @type {[typeof __VLS_components.ElDatePicker, typeof __VLS_components.elDatePicker, ]} */ ;
// @ts-ignore
const __VLS_245 = __VLS_asFunctionalComponent(__VLS_244, new __VLS_244({
    modelValue: (__VLS_ctx.form.validEnd),
    type: "date",
    valueFormat: "YYYY-MM-DD",
    placeholder: "选择日期",
    ...{ style: {} },
}));
const __VLS_246 = __VLS_245({
    modelValue: (__VLS_ctx.form.validEnd),
    type: "date",
    valueFormat: "YYYY-MM-DD",
    placeholder: "选择日期",
    ...{ style: {} },
}, ...__VLS_functionalComponentArgsRest(__VLS_245));
var __VLS_243;
const __VLS_248 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_249 = __VLS_asFunctionalComponent(__VLS_248, new __VLS_248({
    label: "行驶证附件",
    ...{ class: "sf-form-full" },
}));
const __VLS_250 = __VLS_249({
    label: "行驶证附件",
    ...{ class: "sf-form-full" },
}, ...__VLS_functionalComponentArgsRest(__VLS_249));
__VLS_251.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ style: {} },
});
const __VLS_252 = {}.ElUpload;
/** @type {[typeof __VLS_components.ElUpload, typeof __VLS_components.elUpload, typeof __VLS_components.ElUpload, typeof __VLS_components.elUpload, ]} */ ;
// @ts-ignore
const __VLS_253 = __VLS_asFunctionalComponent(__VLS_252, new __VLS_252({
    showFileList: (false),
    beforeUpload: (__VLS_ctx.uploadLicenseFile),
    accept: ".jpg,.jpeg,image/jpeg",
}));
const __VLS_254 = __VLS_253({
    showFileList: (false),
    beforeUpload: (__VLS_ctx.uploadLicenseFile),
    accept: ".jpg,.jpeg,image/jpeg",
}, ...__VLS_functionalComponentArgsRest(__VLS_253));
__VLS_255.slots.default;
const __VLS_256 = {}.ElButton;
/** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
// @ts-ignore
const __VLS_257 = __VLS_asFunctionalComponent(__VLS_256, new __VLS_256({}));
const __VLS_258 = __VLS_257({}, ...__VLS_functionalComponentArgsRest(__VLS_257));
__VLS_259.slots.default;
var __VLS_259;
var __VLS_255;
if (__VLS_ctx.form.licenseFileId) {
    const __VLS_260 = {}.ElLink;
    /** @type {[typeof __VLS_components.ElLink, typeof __VLS_components.elLink, typeof __VLS_components.ElLink, typeof __VLS_components.elLink, ]} */ ;
    // @ts-ignore
    const __VLS_261 = __VLS_asFunctionalComponent(__VLS_260, new __VLS_260({
        href: (__VLS_ctx.fileUrl(__VLS_ctx.form.licenseFileId)),
        target: "_blank",
    }));
    const __VLS_262 = __VLS_261({
        href: (__VLS_ctx.fileUrl(__VLS_ctx.form.licenseFileId)),
        target: "_blank",
    }, ...__VLS_functionalComponentArgsRest(__VLS_261));
    __VLS_263.slots.default;
    var __VLS_263;
}
else {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({
        ...{ class: "sf-muted" },
    });
}
var __VLS_251;
const __VLS_264 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_265 = __VLS_asFunctionalComponent(__VLS_264, new __VLS_264({
    label: "车辆类型",
}));
const __VLS_266 = __VLS_265({
    label: "车辆类型",
}, ...__VLS_functionalComponentArgsRest(__VLS_265));
__VLS_267.slots.default;
const __VLS_268 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_269 = __VLS_asFunctionalComponent(__VLS_268, new __VLS_268({
    modelValue: (__VLS_ctx.form.carType),
    placeholder: "危化/其他",
}));
const __VLS_270 = __VLS_269({
    modelValue: (__VLS_ctx.form.carType),
    placeholder: "危化/其他",
}, ...__VLS_functionalComponentArgsRest(__VLS_269));
var __VLS_267;
var __VLS_199;
{
    const { footer: __VLS_thisSlot } = __VLS_195.slots;
    const __VLS_272 = {}.ElButton;
    /** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
    // @ts-ignore
    const __VLS_273 = __VLS_asFunctionalComponent(__VLS_272, new __VLS_272({
        ...{ 'onClick': {} },
    }));
    const __VLS_274 = __VLS_273({
        ...{ 'onClick': {} },
    }, ...__VLS_functionalComponentArgsRest(__VLS_273));
    let __VLS_276;
    let __VLS_277;
    let __VLS_278;
    const __VLS_279 = {
        onClick: (...[$event]) => {
            __VLS_ctx.dialogVisible = false;
        }
    };
    __VLS_275.slots.default;
    var __VLS_275;
    const __VLS_280 = {}.ElButton;
    /** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
    // @ts-ignore
    const __VLS_281 = __VLS_asFunctionalComponent(__VLS_280, new __VLS_280({
        ...{ 'onClick': {} },
        type: "primary",
        loading: (__VLS_ctx.saving),
    }));
    const __VLS_282 = __VLS_281({
        ...{ 'onClick': {} },
        type: "primary",
        loading: (__VLS_ctx.saving),
    }, ...__VLS_functionalComponentArgsRest(__VLS_281));
    let __VLS_284;
    let __VLS_285;
    let __VLS_286;
    const __VLS_287 = {
        onClick: (__VLS_ctx.onSave)
    };
    __VLS_283.slots.default;
    var __VLS_283;
}
var __VLS_195;
const __VLS_288 = {}.ElDialog;
/** @type {[typeof __VLS_components.ElDialog, typeof __VLS_components.elDialog, typeof __VLS_components.ElDialog, typeof __VLS_components.elDialog, ]} */ ;
// @ts-ignore
const __VLS_289 = __VLS_asFunctionalComponent(__VLS_288, new __VLS_288({
    modelValue: (__VLS_ctx.recordDialogVisible),
    title: (__VLS_ctx.recordEditing?.id ? '编辑出入记录' : '新增出入记录'),
    width: "680px",
}));
const __VLS_290 = __VLS_289({
    modelValue: (__VLS_ctx.recordDialogVisible),
    title: (__VLS_ctx.recordEditing?.id ? '编辑出入记录' : '新增出入记录'),
    width: "680px",
}, ...__VLS_functionalComponentArgsRest(__VLS_289));
__VLS_291.slots.default;
const __VLS_292 = {}.ElForm;
/** @type {[typeof __VLS_components.ElForm, typeof __VLS_components.elForm, typeof __VLS_components.ElForm, typeof __VLS_components.elForm, ]} */ ;
// @ts-ignore
const __VLS_293 = __VLS_asFunctionalComponent(__VLS_292, new __VLS_292({
    labelWidth: "110px",
}));
const __VLS_294 = __VLS_293({
    labelWidth: "110px",
}, ...__VLS_functionalComponentArgsRest(__VLS_293));
__VLS_295.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-form-grid" },
});
const __VLS_296 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_297 = __VLS_asFunctionalComponent(__VLS_296, new __VLS_296({
    label: "企业编码",
}));
const __VLS_298 = __VLS_297({
    label: "企业编码",
}, ...__VLS_functionalComponentArgsRest(__VLS_297));
__VLS_299.slots.default;
const __VLS_300 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_301 = __VLS_asFunctionalComponent(__VLS_300, new __VLS_300({
    modelValue: (__VLS_ctx.recordForm.companyCode),
    placeholder: "可选",
}));
const __VLS_302 = __VLS_301({
    modelValue: (__VLS_ctx.recordForm.companyCode),
    placeholder: "可选",
}, ...__VLS_functionalComponentArgsRest(__VLS_301));
var __VLS_299;
const __VLS_304 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_305 = __VLS_asFunctionalComponent(__VLS_304, new __VLS_304({
    label: "车牌号",
    required: true,
}));
const __VLS_306 = __VLS_305({
    label: "车牌号",
    required: true,
}, ...__VLS_functionalComponentArgsRest(__VLS_305));
__VLS_307.slots.default;
const __VLS_308 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_309 = __VLS_asFunctionalComponent(__VLS_308, new __VLS_308({
    modelValue: (__VLS_ctx.recordForm.licensePlateNumber),
}));
const __VLS_310 = __VLS_309({
    modelValue: (__VLS_ctx.recordForm.licensePlateNumber),
}, ...__VLS_functionalComponentArgsRest(__VLS_309));
var __VLS_307;
const __VLS_312 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_313 = __VLS_asFunctionalComponent(__VLS_312, new __VLS_312({
    label: "车辆类型",
}));
const __VLS_314 = __VLS_313({
    label: "车辆类型",
}, ...__VLS_functionalComponentArgsRest(__VLS_313));
__VLS_315.slots.default;
const __VLS_316 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_317 = __VLS_asFunctionalComponent(__VLS_316, new __VLS_316({
    modelValue: (__VLS_ctx.recordForm.carType),
}));
const __VLS_318 = __VLS_317({
    modelValue: (__VLS_ctx.recordForm.carType),
}, ...__VLS_functionalComponentArgsRest(__VLS_317));
var __VLS_315;
const __VLS_320 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_321 = __VLS_asFunctionalComponent(__VLS_320, new __VLS_320({
    label: "进出状态",
    required: true,
}));
const __VLS_322 = __VLS_321({
    label: "进出状态",
    required: true,
}, ...__VLS_functionalComponentArgsRest(__VLS_321));
__VLS_323.slots.default;
const __VLS_324 = {}.ElSelect;
/** @type {[typeof __VLS_components.ElSelect, typeof __VLS_components.elSelect, typeof __VLS_components.ElSelect, typeof __VLS_components.elSelect, ]} */ ;
// @ts-ignore
const __VLS_325 = __VLS_asFunctionalComponent(__VLS_324, new __VLS_324({
    modelValue: (__VLS_ctx.recordForm.inOutState),
    ...{ style: {} },
}));
const __VLS_326 = __VLS_325({
    modelValue: (__VLS_ctx.recordForm.inOutState),
    ...{ style: {} },
}, ...__VLS_functionalComponentArgsRest(__VLS_325));
__VLS_327.slots.default;
const __VLS_328 = {}.ElOption;
/** @type {[typeof __VLS_components.ElOption, typeof __VLS_components.elOption, ]} */ ;
// @ts-ignore
const __VLS_329 = __VLS_asFunctionalComponent(__VLS_328, new __VLS_328({
    label: "IN",
    value: "IN",
}));
const __VLS_330 = __VLS_329({
    label: "IN",
    value: "IN",
}, ...__VLS_functionalComponentArgsRest(__VLS_329));
const __VLS_332 = {}.ElOption;
/** @type {[typeof __VLS_components.ElOption, typeof __VLS_components.elOption, ]} */ ;
// @ts-ignore
const __VLS_333 = __VLS_asFunctionalComponent(__VLS_332, new __VLS_332({
    label: "OUT",
    value: "OUT",
}));
const __VLS_334 = __VLS_333({
    label: "OUT",
    value: "OUT",
}, ...__VLS_functionalComponentArgsRest(__VLS_333));
var __VLS_327;
var __VLS_323;
const __VLS_336 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_337 = __VLS_asFunctionalComponent(__VLS_336, new __VLS_336({
    label: "进出时间",
    required: true,
}));
const __VLS_338 = __VLS_337({
    label: "进出时间",
    required: true,
}, ...__VLS_functionalComponentArgsRest(__VLS_337));
__VLS_339.slots.default;
const __VLS_340 = {}.ElDatePicker;
/** @type {[typeof __VLS_components.ElDatePicker, typeof __VLS_components.elDatePicker, ]} */ ;
// @ts-ignore
const __VLS_341 = __VLS_asFunctionalComponent(__VLS_340, new __VLS_340({
    modelValue: (__VLS_ctx.recordForm.inOutTime),
    type: "datetime",
    valueFormat: "x",
    placeholder: "选择时间",
    ...{ style: {} },
}));
const __VLS_342 = __VLS_341({
    modelValue: (__VLS_ctx.recordForm.inOutTime),
    type: "datetime",
    valueFormat: "x",
    placeholder: "选择时间",
    ...{ style: {} },
}, ...__VLS_functionalComponentArgsRest(__VLS_341));
var __VLS_339;
const __VLS_344 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_345 = __VLS_asFunctionalComponent(__VLS_344, new __VLS_344({
    label: "图片",
    ...{ class: "sf-form-full" },
}));
const __VLS_346 = __VLS_345({
    label: "图片",
    ...{ class: "sf-form-full" },
}, ...__VLS_functionalComponentArgsRest(__VLS_345));
__VLS_347.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ style: {} },
});
const __VLS_348 = {}.ElUpload;
/** @type {[typeof __VLS_components.ElUpload, typeof __VLS_components.elUpload, typeof __VLS_components.ElUpload, typeof __VLS_components.elUpload, ]} */ ;
// @ts-ignore
const __VLS_349 = __VLS_asFunctionalComponent(__VLS_348, new __VLS_348({
    showFileList: (false),
    beforeUpload: (__VLS_ctx.uploadRecordImage),
    accept: ".jpg,.jpeg,image/jpeg",
}));
const __VLS_350 = __VLS_349({
    showFileList: (false),
    beforeUpload: (__VLS_ctx.uploadRecordImage),
    accept: ".jpg,.jpeg,image/jpeg",
}, ...__VLS_functionalComponentArgsRest(__VLS_349));
__VLS_351.slots.default;
const __VLS_352 = {}.ElButton;
/** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
// @ts-ignore
const __VLS_353 = __VLS_asFunctionalComponent(__VLS_352, new __VLS_352({}));
const __VLS_354 = __VLS_353({}, ...__VLS_functionalComponentArgsRest(__VLS_353));
__VLS_355.slots.default;
var __VLS_355;
var __VLS_351;
if (__VLS_ctx.recordForm.imageFileId) {
    const __VLS_356 = {}.ElLink;
    /** @type {[typeof __VLS_components.ElLink, typeof __VLS_components.elLink, typeof __VLS_components.ElLink, typeof __VLS_components.elLink, ]} */ ;
    // @ts-ignore
    const __VLS_357 = __VLS_asFunctionalComponent(__VLS_356, new __VLS_356({
        href: (__VLS_ctx.fileUrl(__VLS_ctx.recordForm.imageFileId)),
        target: "_blank",
    }));
    const __VLS_358 = __VLS_357({
        href: (__VLS_ctx.fileUrl(__VLS_ctx.recordForm.imageFileId)),
        target: "_blank",
    }, ...__VLS_functionalComponentArgsRest(__VLS_357));
    __VLS_359.slots.default;
    var __VLS_359;
}
else {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({
        ...{ class: "sf-muted" },
    });
}
var __VLS_347;
var __VLS_295;
{
    const { footer: __VLS_thisSlot } = __VLS_291.slots;
    const __VLS_360 = {}.ElButton;
    /** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
    // @ts-ignore
    const __VLS_361 = __VLS_asFunctionalComponent(__VLS_360, new __VLS_360({
        ...{ 'onClick': {} },
    }));
    const __VLS_362 = __VLS_361({
        ...{ 'onClick': {} },
    }, ...__VLS_functionalComponentArgsRest(__VLS_361));
    let __VLS_364;
    let __VLS_365;
    let __VLS_366;
    const __VLS_367 = {
        onClick: (...[$event]) => {
            __VLS_ctx.recordDialogVisible = false;
        }
    };
    __VLS_363.slots.default;
    var __VLS_363;
    const __VLS_368 = {}.ElButton;
    /** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
    // @ts-ignore
    const __VLS_369 = __VLS_asFunctionalComponent(__VLS_368, new __VLS_368({
        ...{ 'onClick': {} },
        type: "primary",
        loading: (__VLS_ctx.recordSaving),
    }));
    const __VLS_370 = __VLS_369({
        ...{ 'onClick': {} },
        type: "primary",
        loading: (__VLS_ctx.recordSaving),
    }, ...__VLS_functionalComponentArgsRest(__VLS_369));
    let __VLS_372;
    let __VLS_373;
    let __VLS_374;
    const __VLS_375 = {
        onClick: (__VLS_ctx.saveRecord)
    };
    __VLS_371.slots.default;
    var __VLS_371;
}
var __VLS_291;
/** @type {__VLS_StyleScopedClasses['sf-page']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-page-head']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-page-title']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-page-sub']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-page-actions']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-card']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-table-card']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-muted']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-filter']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-muted']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-form-grid']} */ ;
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
            loadingCars: loadingCars,
            cars: cars,
            carsTotal: carsTotal,
            carsPage: carsPage,
            carsPageSize: carsPageSize,
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
            loadCars: loadCars,
            loadRecords: loadRecords,
            openRecordCreate: openRecordCreate,
            openRecordEdit: openRecordEdit,
            uploadRecordImage: uploadRecordImage,
            saveRecord: saveRecord,
            deleteRecord: deleteRecord,
            importCars: importCars,
            openCreate: openCreate,
            openEdit: openEdit,
            uploadLicenseFile: uploadLicenseFile,
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
