import { reactive, ref, watch } from "vue";
import { ElMessage, ElMessageBox } from "element-plus";
import { http, rawHttp } from "@/api/http";
const tab = ref("store");
const keyword = ref("");
const loadingStores = ref(false);
const stores = ref([]);
const storesTotal = ref(0);
const storesPage = ref(1);
const storesPageSize = ref(20);
const storeroomStoreNum = ref("");
const loadingStorerooms = ref(false);
const storerooms = ref([]);
const storeroomsTotal = ref(0);
const storeroomsPage = ref(1);
const storeroomsPageSize = ref(20);
const storeDialog = ref(false);
const savingStore = ref(false);
const storeEditing = ref(null);
const storeForm = reactive({
    companyCode: "",
    storeNum: "",
    storeName: "",
    area: null,
    dangerLevel: "",
    quotaDosage: null,
    quotaPeople: null
});
const storeroomDialog = ref(false);
const savingStoreroom = ref(false);
const storeroomEditing = ref(null);
const storeroomForm = reactive({
    storeNum: "",
    storeroomNum: "",
    storeroomName: "",
    area: null,
    dangerLevel: "",
    quotaDosage: null,
    quotaPeople: null
});
async function loadStores() {
    loadingStores.value = true;
    try {
        const data = await http.get(`/api/v1/stores?page=${storesPage.value}&pageSize=${storesPageSize.value}${keyword.value ? `&keyword=${encodeURIComponent(keyword.value)}` : ""}`);
        stores.value = data.list;
        storesTotal.value = data.total;
    }
    catch (e) {
        ElMessage.error(e?.message || "加载失败");
    }
    finally {
        loadingStores.value = false;
    }
}
async function loadStorerooms() {
    loadingStorerooms.value = true;
    try {
        const data = await http.get(`/api/v1/storerooms?page=${storeroomsPage.value}&pageSize=${storeroomsPageSize.value}${storeroomStoreNum.value ? `&storeNum=${encodeURIComponent(storeroomStoreNum.value)}` : ""}`);
        storerooms.value = data.list;
        storeroomsTotal.value = data.total;
    }
    catch (e) {
        ElMessage.error(e?.message || "加载失败");
    }
    finally {
        loadingStorerooms.value = false;
    }
}
function filterStorerooms(storeNum) {
    tab.value = "storeroom";
    storeroomStoreNum.value = storeNum;
    storeroomsPage.value = 1;
    loadStorerooms();
}
function openCreateStore() {
    storeEditing.value = null;
    Object.assign(storeForm, {
        companyCode: "",
        storeNum: "",
        storeName: "",
        area: null,
        dangerLevel: "",
        quotaDosage: null,
        quotaPeople: null
    });
    storeDialog.value = true;
}
function openEditStore(row) {
    storeEditing.value = row;
    Object.assign(storeForm, {
        companyCode: row.companyCode,
        storeNum: row.storeNum,
        storeName: row.storeName,
        area: row.area ?? null,
        dangerLevel: row.dangerLevel || "",
        quotaDosage: row.quotaDosage ?? null,
        quotaPeople: row.quotaPeople ?? null
    });
    storeDialog.value = true;
}
async function saveStore() {
    if (!storeForm.companyCode || !storeForm.storeNum || !storeForm.storeName) {
        ElMessage.warning("请填写必填项");
        return;
    }
    savingStore.value = true;
    try {
        if (!storeEditing.value?.id) {
            await http.post("/api/v1/stores", { ...storeForm });
        }
        else {
            await http.put("/api/v1/stores", { id: storeEditing.value.id, ...storeForm });
        }
        ElMessage.success("保存成功");
        storeDialog.value = false;
        await loadStores();
    }
    catch (e) {
        ElMessage.error(e?.message || "保存失败");
    }
    finally {
        savingStore.value = false;
    }
}
async function deleteStore(id) {
    const ok = await ElMessageBox.confirm("确认删除该仓库？", "提示", { type: "warning" }).catch(() => null);
    if (!ok)
        return;
    try {
        await http.del(`/api/v1/stores/${id}`);
        ElMessage.success("删除成功");
        await loadStores();
    }
    catch (e) {
        ElMessage.error(e?.message || "删除失败");
    }
}
function openCreateStoreroom() {
    storeroomEditing.value = null;
    Object.assign(storeroomForm, {
        storeNum: storeroomStoreNum.value || "",
        storeroomNum: "",
        storeroomName: "",
        area: null,
        dangerLevel: "",
        quotaDosage: null,
        quotaPeople: null
    });
    storeroomDialog.value = true;
}
function openEditStoreroom(row) {
    storeroomEditing.value = row;
    Object.assign(storeroomForm, {
        storeNum: row.storeNum,
        storeroomNum: row.storeroomNum,
        storeroomName: row.storeroomName,
        area: row.area ?? null,
        dangerLevel: row.dangerLevel || "",
        quotaDosage: row.quotaDosage ?? null,
        quotaPeople: row.quotaPeople ?? null
    });
    storeroomDialog.value = true;
}
async function saveStoreroom() {
    if (!storeroomForm.storeNum || !storeroomForm.storeroomNum || !storeroomForm.storeroomName) {
        ElMessage.warning("请填写必填项");
        return;
    }
    savingStoreroom.value = true;
    try {
        if (!storeroomEditing.value?.id) {
            await http.post("/api/v1/storerooms", { ...storeroomForm });
        }
        else {
            await http.put("/api/v1/storerooms", { id: storeroomEditing.value.id, ...storeroomForm });
        }
        ElMessage.success("保存成功");
        storeroomDialog.value = false;
        await loadStorerooms();
    }
    catch (e) {
        ElMessage.error(e?.message || "保存失败");
    }
    finally {
        savingStoreroom.value = false;
    }
}
async function deleteStoreroom(id) {
    const ok = await ElMessageBox.confirm("确认删除该库房？", "提示", { type: "warning" }).catch(() => null);
    if (!ok)
        return;
    try {
        await http.del(`/api/v1/storerooms/${id}`);
        ElMessage.success("删除成功");
        await loadStorerooms();
    }
    catch (e) {
        ElMessage.error(e?.message || "删除失败");
    }
}
async function importStores(file) {
    const fd = new FormData();
    fd.append("file", file);
    try {
        const res = await rawHttp.post("/api/v1/stores/import", fd, {
            headers: { "Content-Type": "multipart/form-data" }
        });
        ElMessage.success(`导入完成：成功 ${res.data.successCount} 条，失败 ${res.data.failCount} 条`);
        await loadStores();
    }
    catch (e) {
        ElMessage.error(e?.message || "导入失败");
    }
    return false;
}
async function importStorerooms(file) {
    const fd = new FormData();
    fd.append("file", file);
    try {
        const res = await rawHttp.post("/api/v1/storerooms/import", fd, {
            headers: { "Content-Type": "multipart/form-data" }
        });
        ElMessage.success(`导入完成：成功 ${res.data.successCount} 条，失败 ${res.data.failCount} 条`);
        await loadStorerooms();
    }
    catch (e) {
        ElMessage.error(e?.message || "导入失败");
    }
    return false;
}
watch(keyword, () => {
    storesPage.value = 1;
    loadStores();
});
watch([storesPage, storesPageSize], () => loadStores());
watch([storeroomsPage, storeroomsPageSize], () => loadStorerooms());
watch(tab, () => {
    if (tab.value === "storeroom")
        loadStorerooms();
});
loadStores();
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
__VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({
    ...{ class: "sf-badge" },
});
(__VLS_ctx.storesTotal);
__VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({
    ...{ class: "sf-badge" },
});
(__VLS_ctx.storeroomsTotal);
const __VLS_0 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_1 = __VLS_asFunctionalComponent(__VLS_0, new __VLS_0({
    modelValue: (__VLS_ctx.keyword),
    placeholder: "编码/名称/企业编码",
    ...{ style: {} },
    clearable: true,
}));
const __VLS_2 = __VLS_1({
    modelValue: (__VLS_ctx.keyword),
    placeholder: "编码/名称/企业编码",
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
    onClick: (__VLS_ctx.openCreateStore)
};
__VLS_7.slots.default;
var __VLS_7;
const __VLS_12 = {}.ElUpload;
/** @type {[typeof __VLS_components.ElUpload, typeof __VLS_components.elUpload, typeof __VLS_components.ElUpload, typeof __VLS_components.elUpload, ]} */ ;
// @ts-ignore
const __VLS_13 = __VLS_asFunctionalComponent(__VLS_12, new __VLS_12({
    showFileList: (false),
    beforeUpload: (__VLS_ctx.importStores),
    accept: ".csv",
}));
const __VLS_14 = __VLS_13({
    showFileList: (false),
    beforeUpload: (__VLS_ctx.importStores),
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
    onClick: (__VLS_ctx.loadStores)
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
    label: "仓库",
    name: "store",
}));
const __VLS_34 = __VLS_33({
    label: "仓库",
    name: "store",
}, ...__VLS_functionalComponentArgsRest(__VLS_33));
__VLS_35.slots.default;
const __VLS_36 = {}.ElTable;
/** @type {[typeof __VLS_components.ElTable, typeof __VLS_components.elTable, typeof __VLS_components.ElTable, typeof __VLS_components.elTable, ]} */ ;
// @ts-ignore
const __VLS_37 = __VLS_asFunctionalComponent(__VLS_36, new __VLS_36({
    data: (__VLS_ctx.stores),
    ...{ style: {} },
}));
const __VLS_38 = __VLS_37({
    data: (__VLS_ctx.stores),
    ...{ style: {} },
}, ...__VLS_functionalComponentArgsRest(__VLS_37));
__VLS_asFunctionalDirective(__VLS_directives.vLoading)(null, { ...__VLS_directiveBindingRestFields, value: (__VLS_ctx.loadingStores) }, null, null);
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
    prop: "storeNum",
    label: "仓库编码",
    minWidth: "140",
}));
const __VLS_46 = __VLS_45({
    prop: "storeNum",
    label: "仓库编码",
    minWidth: "140",
}, ...__VLS_functionalComponentArgsRest(__VLS_45));
const __VLS_48 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_49 = __VLS_asFunctionalComponent(__VLS_48, new __VLS_48({
    prop: "storeName",
    label: "仓库名称",
    minWidth: "180",
}));
const __VLS_50 = __VLS_49({
    prop: "storeName",
    label: "仓库名称",
    minWidth: "180",
}, ...__VLS_functionalComponentArgsRest(__VLS_49));
const __VLS_52 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_53 = __VLS_asFunctionalComponent(__VLS_52, new __VLS_52({
    prop: "area",
    label: "仓库面积（m²）",
    width: "140",
}));
const __VLS_54 = __VLS_53({
    prop: "area",
    label: "仓库面积（m²）",
    width: "140",
}, ...__VLS_functionalComponentArgsRest(__VLS_53));
const __VLS_56 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_57 = __VLS_asFunctionalComponent(__VLS_56, new __VLS_56({
    prop: "dangerLevel",
    label: "危险等级",
    width: "110",
}));
const __VLS_58 = __VLS_57({
    prop: "dangerLevel",
    label: "危险等级",
    width: "110",
}, ...__VLS_functionalComponentArgsRest(__VLS_57));
const __VLS_60 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_61 = __VLS_asFunctionalComponent(__VLS_60, new __VLS_60({
    prop: "quotaDosage",
    label: "核定药量（kg）",
    width: "130",
}));
const __VLS_62 = __VLS_61({
    prop: "quotaDosage",
    label: "核定药量（kg）",
    width: "130",
}, ...__VLS_functionalComponentArgsRest(__VLS_61));
const __VLS_64 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_65 = __VLS_asFunctionalComponent(__VLS_64, new __VLS_64({
    prop: "quotaPeople",
    label: "核定人数（人）",
    width: "130",
}));
const __VLS_66 = __VLS_65({
    prop: "quotaPeople",
    label: "核定人数（人）",
    width: "130",
}, ...__VLS_functionalComponentArgsRest(__VLS_65));
const __VLS_68 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_69 = __VLS_asFunctionalComponent(__VLS_68, new __VLS_68({
    label: "操作",
    width: "200",
    fixed: "right",
}));
const __VLS_70 = __VLS_69({
    label: "操作",
    width: "200",
    fixed: "right",
}, ...__VLS_functionalComponentArgsRest(__VLS_69));
__VLS_71.slots.default;
{
    const { default: __VLS_thisSlot } = __VLS_71.slots;
    const [{ row }] = __VLS_getSlotParams(__VLS_thisSlot);
    const __VLS_72 = {}.ElButton;
    /** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
    // @ts-ignore
    const __VLS_73 = __VLS_asFunctionalComponent(__VLS_72, new __VLS_72({
        ...{ 'onClick': {} },
        size: "small",
    }));
    const __VLS_74 = __VLS_73({
        ...{ 'onClick': {} },
        size: "small",
    }, ...__VLS_functionalComponentArgsRest(__VLS_73));
    let __VLS_76;
    let __VLS_77;
    let __VLS_78;
    const __VLS_79 = {
        onClick: (...[$event]) => {
            __VLS_ctx.openEditStore(row);
        }
    };
    __VLS_75.slots.default;
    var __VLS_75;
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
            __VLS_ctx.filterStorerooms(row.storeNum);
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
        type: "danger",
        plain: true,
    }));
    const __VLS_90 = __VLS_89({
        ...{ 'onClick': {} },
        size: "small",
        type: "danger",
        plain: true,
    }, ...__VLS_functionalComponentArgsRest(__VLS_89));
    let __VLS_92;
    let __VLS_93;
    let __VLS_94;
    const __VLS_95 = {
        onClick: (...[$event]) => {
            __VLS_ctx.deleteStore(row.id);
        }
    };
    __VLS_91.slots.default;
    var __VLS_91;
}
var __VLS_71;
var __VLS_39;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ style: {} },
});
const __VLS_96 = {}.ElPagination;
/** @type {[typeof __VLS_components.ElPagination, typeof __VLS_components.elPagination, ]} */ ;
// @ts-ignore
const __VLS_97 = __VLS_asFunctionalComponent(__VLS_96, new __VLS_96({
    ...{ 'onChange': {} },
    background: true,
    layout: "prev, pager, next, sizes, total",
    total: (__VLS_ctx.storesTotal),
    currentPage: (__VLS_ctx.storesPage),
    pageSize: (__VLS_ctx.storesPageSize),
}));
const __VLS_98 = __VLS_97({
    ...{ 'onChange': {} },
    background: true,
    layout: "prev, pager, next, sizes, total",
    total: (__VLS_ctx.storesTotal),
    currentPage: (__VLS_ctx.storesPage),
    pageSize: (__VLS_ctx.storesPageSize),
}, ...__VLS_functionalComponentArgsRest(__VLS_97));
let __VLS_100;
let __VLS_101;
let __VLS_102;
const __VLS_103 = {
    onChange: (__VLS_ctx.loadStores)
};
var __VLS_99;
var __VLS_35;
const __VLS_104 = {}.ElTabPane;
/** @type {[typeof __VLS_components.ElTabPane, typeof __VLS_components.elTabPane, typeof __VLS_components.ElTabPane, typeof __VLS_components.elTabPane, ]} */ ;
// @ts-ignore
const __VLS_105 = __VLS_asFunctionalComponent(__VLS_104, new __VLS_104({
    label: "库房",
    name: "storeroom",
}));
const __VLS_106 = __VLS_105({
    label: "库房",
    name: "storeroom",
}, ...__VLS_functionalComponentArgsRest(__VLS_105));
__VLS_107.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-filter" },
    ...{ style: {} },
});
const __VLS_108 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_109 = __VLS_asFunctionalComponent(__VLS_108, new __VLS_108({
    modelValue: (__VLS_ctx.storeroomStoreNum),
    placeholder: "仓库编码（筛选）",
    ...{ style: {} },
    clearable: true,
}));
const __VLS_110 = __VLS_109({
    modelValue: (__VLS_ctx.storeroomStoreNum),
    placeholder: "仓库编码（筛选）",
    ...{ style: {} },
    clearable: true,
}, ...__VLS_functionalComponentArgsRest(__VLS_109));
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
    onClick: (__VLS_ctx.openCreateStoreroom)
};
__VLS_115.slots.default;
var __VLS_115;
const __VLS_120 = {}.ElUpload;
/** @type {[typeof __VLS_components.ElUpload, typeof __VLS_components.elUpload, typeof __VLS_components.ElUpload, typeof __VLS_components.elUpload, ]} */ ;
// @ts-ignore
const __VLS_121 = __VLS_asFunctionalComponent(__VLS_120, new __VLS_120({
    showFileList: (false),
    beforeUpload: (__VLS_ctx.importStorerooms),
    accept: ".csv",
}));
const __VLS_122 = __VLS_121({
    showFileList: (false),
    beforeUpload: (__VLS_ctx.importStorerooms),
    accept: ".csv",
}, ...__VLS_functionalComponentArgsRest(__VLS_121));
__VLS_123.slots.default;
const __VLS_124 = {}.ElButton;
/** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
// @ts-ignore
const __VLS_125 = __VLS_asFunctionalComponent(__VLS_124, new __VLS_124({}));
const __VLS_126 = __VLS_125({}, ...__VLS_functionalComponentArgsRest(__VLS_125));
__VLS_127.slots.default;
var __VLS_127;
var __VLS_123;
const __VLS_128 = {}.ElButton;
/** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
// @ts-ignore
const __VLS_129 = __VLS_asFunctionalComponent(__VLS_128, new __VLS_128({
    ...{ 'onClick': {} },
}));
const __VLS_130 = __VLS_129({
    ...{ 'onClick': {} },
}, ...__VLS_functionalComponentArgsRest(__VLS_129));
let __VLS_132;
let __VLS_133;
let __VLS_134;
const __VLS_135 = {
    onClick: (__VLS_ctx.loadStorerooms)
};
__VLS_131.slots.default;
var __VLS_131;
const __VLS_136 = {}.ElTable;
/** @type {[typeof __VLS_components.ElTable, typeof __VLS_components.elTable, typeof __VLS_components.ElTable, typeof __VLS_components.elTable, ]} */ ;
// @ts-ignore
const __VLS_137 = __VLS_asFunctionalComponent(__VLS_136, new __VLS_136({
    data: (__VLS_ctx.storerooms),
    ...{ style: {} },
}));
const __VLS_138 = __VLS_137({
    data: (__VLS_ctx.storerooms),
    ...{ style: {} },
}, ...__VLS_functionalComponentArgsRest(__VLS_137));
__VLS_asFunctionalDirective(__VLS_directives.vLoading)(null, { ...__VLS_directiveBindingRestFields, value: (__VLS_ctx.loadingStorerooms) }, null, null);
__VLS_139.slots.default;
const __VLS_140 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_141 = __VLS_asFunctionalComponent(__VLS_140, new __VLS_140({
    prop: "storeNum",
    label: "仓库编码",
    minWidth: "140",
}));
const __VLS_142 = __VLS_141({
    prop: "storeNum",
    label: "仓库编码",
    minWidth: "140",
}, ...__VLS_functionalComponentArgsRest(__VLS_141));
const __VLS_144 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_145 = __VLS_asFunctionalComponent(__VLS_144, new __VLS_144({
    prop: "storeroomNum",
    label: "库房编码",
    minWidth: "140",
}));
const __VLS_146 = __VLS_145({
    prop: "storeroomNum",
    label: "库房编码",
    minWidth: "140",
}, ...__VLS_functionalComponentArgsRest(__VLS_145));
const __VLS_148 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_149 = __VLS_asFunctionalComponent(__VLS_148, new __VLS_148({
    prop: "storeroomName",
    label: "库房名称",
    minWidth: "180",
}));
const __VLS_150 = __VLS_149({
    prop: "storeroomName",
    label: "库房名称",
    minWidth: "180",
}, ...__VLS_functionalComponentArgsRest(__VLS_149));
const __VLS_152 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_153 = __VLS_asFunctionalComponent(__VLS_152, new __VLS_152({
    prop: "area",
    label: "库房面积（m²）",
    width: "140",
}));
const __VLS_154 = __VLS_153({
    prop: "area",
    label: "库房面积（m²）",
    width: "140",
}, ...__VLS_functionalComponentArgsRest(__VLS_153));
const __VLS_156 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_157 = __VLS_asFunctionalComponent(__VLS_156, new __VLS_156({
    prop: "dangerLevel",
    label: "危险等级",
    width: "110",
}));
const __VLS_158 = __VLS_157({
    prop: "dangerLevel",
    label: "危险等级",
    width: "110",
}, ...__VLS_functionalComponentArgsRest(__VLS_157));
const __VLS_160 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_161 = __VLS_asFunctionalComponent(__VLS_160, new __VLS_160({
    prop: "quotaDosage",
    label: "核定药量（t）",
    width: "120",
}));
const __VLS_162 = __VLS_161({
    prop: "quotaDosage",
    label: "核定药量（t）",
    width: "120",
}, ...__VLS_functionalComponentArgsRest(__VLS_161));
const __VLS_164 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_165 = __VLS_asFunctionalComponent(__VLS_164, new __VLS_164({
    prop: "quotaPeople",
    label: "核定人数（人）",
    width: "130",
}));
const __VLS_166 = __VLS_165({
    prop: "quotaPeople",
    label: "核定人数（人）",
    width: "130",
}, ...__VLS_functionalComponentArgsRest(__VLS_165));
const __VLS_168 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_169 = __VLS_asFunctionalComponent(__VLS_168, new __VLS_168({
    label: "操作",
    width: "160",
    fixed: "right",
}));
const __VLS_170 = __VLS_169({
    label: "操作",
    width: "160",
    fixed: "right",
}, ...__VLS_functionalComponentArgsRest(__VLS_169));
__VLS_171.slots.default;
{
    const { default: __VLS_thisSlot } = __VLS_171.slots;
    const [{ row }] = __VLS_getSlotParams(__VLS_thisSlot);
    const __VLS_172 = {}.ElButton;
    /** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
    // @ts-ignore
    const __VLS_173 = __VLS_asFunctionalComponent(__VLS_172, new __VLS_172({
        ...{ 'onClick': {} },
        size: "small",
    }));
    const __VLS_174 = __VLS_173({
        ...{ 'onClick': {} },
        size: "small",
    }, ...__VLS_functionalComponentArgsRest(__VLS_173));
    let __VLS_176;
    let __VLS_177;
    let __VLS_178;
    const __VLS_179 = {
        onClick: (...[$event]) => {
            __VLS_ctx.openEditStoreroom(row);
        }
    };
    __VLS_175.slots.default;
    var __VLS_175;
    const __VLS_180 = {}.ElButton;
    /** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
    // @ts-ignore
    const __VLS_181 = __VLS_asFunctionalComponent(__VLS_180, new __VLS_180({
        ...{ 'onClick': {} },
        size: "small",
        type: "danger",
        plain: true,
    }));
    const __VLS_182 = __VLS_181({
        ...{ 'onClick': {} },
        size: "small",
        type: "danger",
        plain: true,
    }, ...__VLS_functionalComponentArgsRest(__VLS_181));
    let __VLS_184;
    let __VLS_185;
    let __VLS_186;
    const __VLS_187 = {
        onClick: (...[$event]) => {
            __VLS_ctx.deleteStoreroom(row.id);
        }
    };
    __VLS_183.slots.default;
    var __VLS_183;
}
var __VLS_171;
var __VLS_139;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ style: {} },
});
const __VLS_188 = {}.ElPagination;
/** @type {[typeof __VLS_components.ElPagination, typeof __VLS_components.elPagination, ]} */ ;
// @ts-ignore
const __VLS_189 = __VLS_asFunctionalComponent(__VLS_188, new __VLS_188({
    ...{ 'onChange': {} },
    background: true,
    layout: "prev, pager, next, sizes, total",
    total: (__VLS_ctx.storeroomsTotal),
    currentPage: (__VLS_ctx.storeroomsPage),
    pageSize: (__VLS_ctx.storeroomsPageSize),
}));
const __VLS_190 = __VLS_189({
    ...{ 'onChange': {} },
    background: true,
    layout: "prev, pager, next, sizes, total",
    total: (__VLS_ctx.storeroomsTotal),
    currentPage: (__VLS_ctx.storeroomsPage),
    pageSize: (__VLS_ctx.storeroomsPageSize),
}, ...__VLS_functionalComponentArgsRest(__VLS_189));
let __VLS_192;
let __VLS_193;
let __VLS_194;
const __VLS_195 = {
    onChange: (__VLS_ctx.loadStorerooms)
};
var __VLS_191;
var __VLS_107;
var __VLS_31;
const __VLS_196 = {}.ElDialog;
/** @type {[typeof __VLS_components.ElDialog, typeof __VLS_components.elDialog, typeof __VLS_components.ElDialog, typeof __VLS_components.elDialog, ]} */ ;
// @ts-ignore
const __VLS_197 = __VLS_asFunctionalComponent(__VLS_196, new __VLS_196({
    modelValue: (__VLS_ctx.storeDialog),
    title: (__VLS_ctx.storeEditing?.id ? '编辑仓库' : '新增仓库'),
    width: "640px",
}));
const __VLS_198 = __VLS_197({
    modelValue: (__VLS_ctx.storeDialog),
    title: (__VLS_ctx.storeEditing?.id ? '编辑仓库' : '新增仓库'),
    width: "640px",
}, ...__VLS_functionalComponentArgsRest(__VLS_197));
__VLS_199.slots.default;
const __VLS_200 = {}.ElForm;
/** @type {[typeof __VLS_components.ElForm, typeof __VLS_components.elForm, typeof __VLS_components.ElForm, typeof __VLS_components.elForm, ]} */ ;
// @ts-ignore
const __VLS_201 = __VLS_asFunctionalComponent(__VLS_200, new __VLS_200({
    labelWidth: "100px",
}));
const __VLS_202 = __VLS_201({
    labelWidth: "100px",
}, ...__VLS_functionalComponentArgsRest(__VLS_201));
__VLS_203.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-form-grid" },
});
const __VLS_204 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_205 = __VLS_asFunctionalComponent(__VLS_204, new __VLS_204({
    label: "企业编码",
    required: true,
}));
const __VLS_206 = __VLS_205({
    label: "企业编码",
    required: true,
}, ...__VLS_functionalComponentArgsRest(__VLS_205));
__VLS_207.slots.default;
const __VLS_208 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_209 = __VLS_asFunctionalComponent(__VLS_208, new __VLS_208({
    modelValue: (__VLS_ctx.storeForm.companyCode),
}));
const __VLS_210 = __VLS_209({
    modelValue: (__VLS_ctx.storeForm.companyCode),
}, ...__VLS_functionalComponentArgsRest(__VLS_209));
var __VLS_207;
const __VLS_212 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_213 = __VLS_asFunctionalComponent(__VLS_212, new __VLS_212({
    label: "仓库编码",
    required: true,
}));
const __VLS_214 = __VLS_213({
    label: "仓库编码",
    required: true,
}, ...__VLS_functionalComponentArgsRest(__VLS_213));
__VLS_215.slots.default;
const __VLS_216 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_217 = __VLS_asFunctionalComponent(__VLS_216, new __VLS_216({
    modelValue: (__VLS_ctx.storeForm.storeNum),
}));
const __VLS_218 = __VLS_217({
    modelValue: (__VLS_ctx.storeForm.storeNum),
}, ...__VLS_functionalComponentArgsRest(__VLS_217));
var __VLS_215;
const __VLS_220 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_221 = __VLS_asFunctionalComponent(__VLS_220, new __VLS_220({
    label: "仓库名称",
    required: true,
}));
const __VLS_222 = __VLS_221({
    label: "仓库名称",
    required: true,
}, ...__VLS_functionalComponentArgsRest(__VLS_221));
__VLS_223.slots.default;
const __VLS_224 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_225 = __VLS_asFunctionalComponent(__VLS_224, new __VLS_224({
    modelValue: (__VLS_ctx.storeForm.storeName),
}));
const __VLS_226 = __VLS_225({
    modelValue: (__VLS_ctx.storeForm.storeName),
}, ...__VLS_functionalComponentArgsRest(__VLS_225));
var __VLS_223;
const __VLS_228 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_229 = __VLS_asFunctionalComponent(__VLS_228, new __VLS_228({
    label: "仓库面积（m²）",
}));
const __VLS_230 = __VLS_229({
    label: "仓库面积（m²）",
}, ...__VLS_functionalComponentArgsRest(__VLS_229));
__VLS_231.slots.default;
const __VLS_232 = {}.ElInputNumber;
/** @type {[typeof __VLS_components.ElInputNumber, typeof __VLS_components.elInputNumber, ]} */ ;
// @ts-ignore
const __VLS_233 = __VLS_asFunctionalComponent(__VLS_232, new __VLS_232({
    modelValue: (__VLS_ctx.storeForm.area),
    ...{ style: {} },
}));
const __VLS_234 = __VLS_233({
    modelValue: (__VLS_ctx.storeForm.area),
    ...{ style: {} },
}, ...__VLS_functionalComponentArgsRest(__VLS_233));
var __VLS_231;
const __VLS_236 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_237 = __VLS_asFunctionalComponent(__VLS_236, new __VLS_236({
    label: "危险等级",
}));
const __VLS_238 = __VLS_237({
    label: "危险等级",
}, ...__VLS_functionalComponentArgsRest(__VLS_237));
__VLS_239.slots.default;
const __VLS_240 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_241 = __VLS_asFunctionalComponent(__VLS_240, new __VLS_240({
    modelValue: (__VLS_ctx.storeForm.dangerLevel),
    placeholder: "01/02/03",
}));
const __VLS_242 = __VLS_241({
    modelValue: (__VLS_ctx.storeForm.dangerLevel),
    placeholder: "01/02/03",
}, ...__VLS_functionalComponentArgsRest(__VLS_241));
var __VLS_239;
const __VLS_244 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_245 = __VLS_asFunctionalComponent(__VLS_244, new __VLS_244({
    label: "核定药量（kg）",
}));
const __VLS_246 = __VLS_245({
    label: "核定药量（kg）",
}, ...__VLS_functionalComponentArgsRest(__VLS_245));
__VLS_247.slots.default;
const __VLS_248 = {}.ElInputNumber;
/** @type {[typeof __VLS_components.ElInputNumber, typeof __VLS_components.elInputNumber, ]} */ ;
// @ts-ignore
const __VLS_249 = __VLS_asFunctionalComponent(__VLS_248, new __VLS_248({
    modelValue: (__VLS_ctx.storeForm.quotaDosage),
    ...{ style: {} },
}));
const __VLS_250 = __VLS_249({
    modelValue: (__VLS_ctx.storeForm.quotaDosage),
    ...{ style: {} },
}, ...__VLS_functionalComponentArgsRest(__VLS_249));
var __VLS_247;
const __VLS_252 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_253 = __VLS_asFunctionalComponent(__VLS_252, new __VLS_252({
    label: "核定人数（人）",
}));
const __VLS_254 = __VLS_253({
    label: "核定人数（人）",
}, ...__VLS_functionalComponentArgsRest(__VLS_253));
__VLS_255.slots.default;
const __VLS_256 = {}.ElInputNumber;
/** @type {[typeof __VLS_components.ElInputNumber, typeof __VLS_components.elInputNumber, ]} */ ;
// @ts-ignore
const __VLS_257 = __VLS_asFunctionalComponent(__VLS_256, new __VLS_256({
    modelValue: (__VLS_ctx.storeForm.quotaPeople),
    ...{ style: {} },
}));
const __VLS_258 = __VLS_257({
    modelValue: (__VLS_ctx.storeForm.quotaPeople),
    ...{ style: {} },
}, ...__VLS_functionalComponentArgsRest(__VLS_257));
var __VLS_255;
var __VLS_203;
{
    const { footer: __VLS_thisSlot } = __VLS_199.slots;
    const __VLS_260 = {}.ElButton;
    /** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
    // @ts-ignore
    const __VLS_261 = __VLS_asFunctionalComponent(__VLS_260, new __VLS_260({
        ...{ 'onClick': {} },
    }));
    const __VLS_262 = __VLS_261({
        ...{ 'onClick': {} },
    }, ...__VLS_functionalComponentArgsRest(__VLS_261));
    let __VLS_264;
    let __VLS_265;
    let __VLS_266;
    const __VLS_267 = {
        onClick: (...[$event]) => {
            __VLS_ctx.storeDialog = false;
        }
    };
    __VLS_263.slots.default;
    var __VLS_263;
    const __VLS_268 = {}.ElButton;
    /** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
    // @ts-ignore
    const __VLS_269 = __VLS_asFunctionalComponent(__VLS_268, new __VLS_268({
        ...{ 'onClick': {} },
        type: "primary",
        loading: (__VLS_ctx.savingStore),
    }));
    const __VLS_270 = __VLS_269({
        ...{ 'onClick': {} },
        type: "primary",
        loading: (__VLS_ctx.savingStore),
    }, ...__VLS_functionalComponentArgsRest(__VLS_269));
    let __VLS_272;
    let __VLS_273;
    let __VLS_274;
    const __VLS_275 = {
        onClick: (__VLS_ctx.saveStore)
    };
    __VLS_271.slots.default;
    var __VLS_271;
}
var __VLS_199;
const __VLS_276 = {}.ElDialog;
/** @type {[typeof __VLS_components.ElDialog, typeof __VLS_components.elDialog, typeof __VLS_components.ElDialog, typeof __VLS_components.elDialog, ]} */ ;
// @ts-ignore
const __VLS_277 = __VLS_asFunctionalComponent(__VLS_276, new __VLS_276({
    modelValue: (__VLS_ctx.storeroomDialog),
    title: (__VLS_ctx.storeroomEditing?.id ? '编辑库房' : '新增库房'),
    width: "640px",
}));
const __VLS_278 = __VLS_277({
    modelValue: (__VLS_ctx.storeroomDialog),
    title: (__VLS_ctx.storeroomEditing?.id ? '编辑库房' : '新增库房'),
    width: "640px",
}, ...__VLS_functionalComponentArgsRest(__VLS_277));
__VLS_279.slots.default;
const __VLS_280 = {}.ElForm;
/** @type {[typeof __VLS_components.ElForm, typeof __VLS_components.elForm, typeof __VLS_components.ElForm, typeof __VLS_components.elForm, ]} */ ;
// @ts-ignore
const __VLS_281 = __VLS_asFunctionalComponent(__VLS_280, new __VLS_280({
    labelWidth: "100px",
}));
const __VLS_282 = __VLS_281({
    labelWidth: "100px",
}, ...__VLS_functionalComponentArgsRest(__VLS_281));
__VLS_283.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-form-grid" },
});
const __VLS_284 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_285 = __VLS_asFunctionalComponent(__VLS_284, new __VLS_284({
    label: "仓库编码",
    required: true,
}));
const __VLS_286 = __VLS_285({
    label: "仓库编码",
    required: true,
}, ...__VLS_functionalComponentArgsRest(__VLS_285));
__VLS_287.slots.default;
const __VLS_288 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_289 = __VLS_asFunctionalComponent(__VLS_288, new __VLS_288({
    modelValue: (__VLS_ctx.storeroomForm.storeNum),
}));
const __VLS_290 = __VLS_289({
    modelValue: (__VLS_ctx.storeroomForm.storeNum),
}, ...__VLS_functionalComponentArgsRest(__VLS_289));
var __VLS_287;
const __VLS_292 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_293 = __VLS_asFunctionalComponent(__VLS_292, new __VLS_292({
    label: "库房编码",
    required: true,
}));
const __VLS_294 = __VLS_293({
    label: "库房编码",
    required: true,
}, ...__VLS_functionalComponentArgsRest(__VLS_293));
__VLS_295.slots.default;
const __VLS_296 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_297 = __VLS_asFunctionalComponent(__VLS_296, new __VLS_296({
    modelValue: (__VLS_ctx.storeroomForm.storeroomNum),
}));
const __VLS_298 = __VLS_297({
    modelValue: (__VLS_ctx.storeroomForm.storeroomNum),
}, ...__VLS_functionalComponentArgsRest(__VLS_297));
var __VLS_295;
const __VLS_300 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_301 = __VLS_asFunctionalComponent(__VLS_300, new __VLS_300({
    label: "库房名称",
    required: true,
}));
const __VLS_302 = __VLS_301({
    label: "库房名称",
    required: true,
}, ...__VLS_functionalComponentArgsRest(__VLS_301));
__VLS_303.slots.default;
const __VLS_304 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_305 = __VLS_asFunctionalComponent(__VLS_304, new __VLS_304({
    modelValue: (__VLS_ctx.storeroomForm.storeroomName),
}));
const __VLS_306 = __VLS_305({
    modelValue: (__VLS_ctx.storeroomForm.storeroomName),
}, ...__VLS_functionalComponentArgsRest(__VLS_305));
var __VLS_303;
const __VLS_308 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_309 = __VLS_asFunctionalComponent(__VLS_308, new __VLS_308({
    label: "库房面积（m²）",
}));
const __VLS_310 = __VLS_309({
    label: "库房面积（m²）",
}, ...__VLS_functionalComponentArgsRest(__VLS_309));
__VLS_311.slots.default;
const __VLS_312 = {}.ElInputNumber;
/** @type {[typeof __VLS_components.ElInputNumber, typeof __VLS_components.elInputNumber, ]} */ ;
// @ts-ignore
const __VLS_313 = __VLS_asFunctionalComponent(__VLS_312, new __VLS_312({
    modelValue: (__VLS_ctx.storeroomForm.area),
    ...{ style: {} },
}));
const __VLS_314 = __VLS_313({
    modelValue: (__VLS_ctx.storeroomForm.area),
    ...{ style: {} },
}, ...__VLS_functionalComponentArgsRest(__VLS_313));
var __VLS_311;
const __VLS_316 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_317 = __VLS_asFunctionalComponent(__VLS_316, new __VLS_316({
    label: "危险等级",
}));
const __VLS_318 = __VLS_317({
    label: "危险等级",
}, ...__VLS_functionalComponentArgsRest(__VLS_317));
__VLS_319.slots.default;
const __VLS_320 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_321 = __VLS_asFunctionalComponent(__VLS_320, new __VLS_320({
    modelValue: (__VLS_ctx.storeroomForm.dangerLevel),
    placeholder: "01/02/03",
}));
const __VLS_322 = __VLS_321({
    modelValue: (__VLS_ctx.storeroomForm.dangerLevel),
    placeholder: "01/02/03",
}, ...__VLS_functionalComponentArgsRest(__VLS_321));
var __VLS_319;
const __VLS_324 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_325 = __VLS_asFunctionalComponent(__VLS_324, new __VLS_324({
    label: "核定药量（t）",
}));
const __VLS_326 = __VLS_325({
    label: "核定药量（t）",
}, ...__VLS_functionalComponentArgsRest(__VLS_325));
__VLS_327.slots.default;
const __VLS_328 = {}.ElInputNumber;
/** @type {[typeof __VLS_components.ElInputNumber, typeof __VLS_components.elInputNumber, ]} */ ;
// @ts-ignore
const __VLS_329 = __VLS_asFunctionalComponent(__VLS_328, new __VLS_328({
    modelValue: (__VLS_ctx.storeroomForm.quotaDosage),
    ...{ style: {} },
}));
const __VLS_330 = __VLS_329({
    modelValue: (__VLS_ctx.storeroomForm.quotaDosage),
    ...{ style: {} },
}, ...__VLS_functionalComponentArgsRest(__VLS_329));
var __VLS_327;
const __VLS_332 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_333 = __VLS_asFunctionalComponent(__VLS_332, new __VLS_332({
    label: "核定人数（人）",
}));
const __VLS_334 = __VLS_333({
    label: "核定人数（人）",
}, ...__VLS_functionalComponentArgsRest(__VLS_333));
__VLS_335.slots.default;
const __VLS_336 = {}.ElInputNumber;
/** @type {[typeof __VLS_components.ElInputNumber, typeof __VLS_components.elInputNumber, ]} */ ;
// @ts-ignore
const __VLS_337 = __VLS_asFunctionalComponent(__VLS_336, new __VLS_336({
    modelValue: (__VLS_ctx.storeroomForm.quotaPeople),
    ...{ style: {} },
}));
const __VLS_338 = __VLS_337({
    modelValue: (__VLS_ctx.storeroomForm.quotaPeople),
    ...{ style: {} },
}, ...__VLS_functionalComponentArgsRest(__VLS_337));
var __VLS_335;
var __VLS_283;
{
    const { footer: __VLS_thisSlot } = __VLS_279.slots;
    const __VLS_340 = {}.ElButton;
    /** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
    // @ts-ignore
    const __VLS_341 = __VLS_asFunctionalComponent(__VLS_340, new __VLS_340({
        ...{ 'onClick': {} },
    }));
    const __VLS_342 = __VLS_341({
        ...{ 'onClick': {} },
    }, ...__VLS_functionalComponentArgsRest(__VLS_341));
    let __VLS_344;
    let __VLS_345;
    let __VLS_346;
    const __VLS_347 = {
        onClick: (...[$event]) => {
            __VLS_ctx.storeroomDialog = false;
        }
    };
    __VLS_343.slots.default;
    var __VLS_343;
    const __VLS_348 = {}.ElButton;
    /** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
    // @ts-ignore
    const __VLS_349 = __VLS_asFunctionalComponent(__VLS_348, new __VLS_348({
        ...{ 'onClick': {} },
        type: "primary",
        loading: (__VLS_ctx.savingStoreroom),
    }));
    const __VLS_350 = __VLS_349({
        ...{ 'onClick': {} },
        type: "primary",
        loading: (__VLS_ctx.savingStoreroom),
    }, ...__VLS_functionalComponentArgsRest(__VLS_349));
    let __VLS_352;
    let __VLS_353;
    let __VLS_354;
    const __VLS_355 = {
        onClick: (__VLS_ctx.saveStoreroom)
    };
    __VLS_351.slots.default;
    var __VLS_351;
}
var __VLS_279;
/** @type {__VLS_StyleScopedClasses['sf-page']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-page-head']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-page-title']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-page-sub']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-page-actions']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-badge']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-badge']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-card']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-table-card']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-filter']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-form-grid']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-form-grid']} */ ;
var __VLS_dollars;
const __VLS_self = (await import('vue')).defineComponent({
    setup() {
        return {
            tab: tab,
            keyword: keyword,
            loadingStores: loadingStores,
            stores: stores,
            storesTotal: storesTotal,
            storesPage: storesPage,
            storesPageSize: storesPageSize,
            storeroomStoreNum: storeroomStoreNum,
            loadingStorerooms: loadingStorerooms,
            storerooms: storerooms,
            storeroomsTotal: storeroomsTotal,
            storeroomsPage: storeroomsPage,
            storeroomsPageSize: storeroomsPageSize,
            storeDialog: storeDialog,
            savingStore: savingStore,
            storeEditing: storeEditing,
            storeForm: storeForm,
            storeroomDialog: storeroomDialog,
            savingStoreroom: savingStoreroom,
            storeroomEditing: storeroomEditing,
            storeroomForm: storeroomForm,
            loadStores: loadStores,
            loadStorerooms: loadStorerooms,
            filterStorerooms: filterStorerooms,
            openCreateStore: openCreateStore,
            openEditStore: openEditStore,
            saveStore: saveStore,
            deleteStore: deleteStore,
            openCreateStoreroom: openCreateStoreroom,
            openEditStoreroom: openEditStoreroom,
            saveStoreroom: saveStoreroom,
            deleteStoreroom: deleteStoreroom,
            importStores: importStores,
            importStorerooms: importStorerooms,
        };
    },
});
export default (await import('vue')).defineComponent({
    setup() {
        return {};
    },
});
; /* PartiallyEnd: #4569/main.vue */
