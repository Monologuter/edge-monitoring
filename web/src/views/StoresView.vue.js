import { reactive, ref, watch } from "vue";
import { ElMessage, ElMessageBox } from "element-plus";
import { http } from "@/api/http";
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
    Object.assign(storeForm, { companyCode: "", storeNum: "", storeName: "", dangerLevel: "", quotaDosage: null, quotaPeople: null });
    storeDialog.value = true;
}
function openEditStore(row) {
    storeEditing.value = row;
    Object.assign(storeForm, {
        companyCode: row.companyCode,
        storeNum: row.storeNum,
        storeName: row.storeName,
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
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ style: {} },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.h2, __VLS_intrinsicElements.h2)({
    ...{ class: "sf-title" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ style: {} },
});
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
    onClick: (__VLS_ctx.loadStores)
};
__VLS_15.slots.default;
var __VLS_15;
const __VLS_20 = {}.ElTabs;
/** @type {[typeof __VLS_components.ElTabs, typeof __VLS_components.elTabs, typeof __VLS_components.ElTabs, typeof __VLS_components.elTabs, ]} */ ;
// @ts-ignore
const __VLS_21 = __VLS_asFunctionalComponent(__VLS_20, new __VLS_20({
    modelValue: (__VLS_ctx.tab),
}));
const __VLS_22 = __VLS_21({
    modelValue: (__VLS_ctx.tab),
}, ...__VLS_functionalComponentArgsRest(__VLS_21));
__VLS_23.slots.default;
const __VLS_24 = {}.ElTabPane;
/** @type {[typeof __VLS_components.ElTabPane, typeof __VLS_components.elTabPane, typeof __VLS_components.ElTabPane, typeof __VLS_components.elTabPane, ]} */ ;
// @ts-ignore
const __VLS_25 = __VLS_asFunctionalComponent(__VLS_24, new __VLS_24({
    label: "仓库",
    name: "store",
}));
const __VLS_26 = __VLS_25({
    label: "仓库",
    name: "store",
}, ...__VLS_functionalComponentArgsRest(__VLS_25));
__VLS_27.slots.default;
const __VLS_28 = {}.ElTable;
/** @type {[typeof __VLS_components.ElTable, typeof __VLS_components.elTable, typeof __VLS_components.ElTable, typeof __VLS_components.elTable, ]} */ ;
// @ts-ignore
const __VLS_29 = __VLS_asFunctionalComponent(__VLS_28, new __VLS_28({
    data: (__VLS_ctx.stores),
    ...{ style: {} },
}));
const __VLS_30 = __VLS_29({
    data: (__VLS_ctx.stores),
    ...{ style: {} },
}, ...__VLS_functionalComponentArgsRest(__VLS_29));
__VLS_asFunctionalDirective(__VLS_directives.vLoading)(null, { ...__VLS_directiveBindingRestFields, value: (__VLS_ctx.loadingStores) }, null, null);
__VLS_31.slots.default;
const __VLS_32 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_33 = __VLS_asFunctionalComponent(__VLS_32, new __VLS_32({
    prop: "companyCode",
    label: "企业编码",
    minWidth: "140",
}));
const __VLS_34 = __VLS_33({
    prop: "companyCode",
    label: "企业编码",
    minWidth: "140",
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
    prop: "storeName",
    label: "仓库名称",
    minWidth: "180",
}));
const __VLS_42 = __VLS_41({
    prop: "storeName",
    label: "仓库名称",
    minWidth: "180",
}, ...__VLS_functionalComponentArgsRest(__VLS_41));
const __VLS_44 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_45 = __VLS_asFunctionalComponent(__VLS_44, new __VLS_44({
    prop: "dangerLevel",
    label: "危险等级",
    width: "110",
}));
const __VLS_46 = __VLS_45({
    prop: "dangerLevel",
    label: "危险等级",
    width: "110",
}, ...__VLS_functionalComponentArgsRest(__VLS_45));
const __VLS_48 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_49 = __VLS_asFunctionalComponent(__VLS_48, new __VLS_48({
    prop: "quotaDosage",
    label: "核定药量",
    width: "110",
}));
const __VLS_50 = __VLS_49({
    prop: "quotaDosage",
    label: "核定药量",
    width: "110",
}, ...__VLS_functionalComponentArgsRest(__VLS_49));
const __VLS_52 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_53 = __VLS_asFunctionalComponent(__VLS_52, new __VLS_52({
    prop: "quotaPeople",
    label: "核定人数",
    width: "110",
}));
const __VLS_54 = __VLS_53({
    prop: "quotaPeople",
    label: "核定人数",
    width: "110",
}, ...__VLS_functionalComponentArgsRest(__VLS_53));
const __VLS_56 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_57 = __VLS_asFunctionalComponent(__VLS_56, new __VLS_56({
    label: "操作",
    width: "200",
    fixed: "right",
}));
const __VLS_58 = __VLS_57({
    label: "操作",
    width: "200",
    fixed: "right",
}, ...__VLS_functionalComponentArgsRest(__VLS_57));
__VLS_59.slots.default;
{
    const { default: __VLS_thisSlot } = __VLS_59.slots;
    const [{ row }] = __VLS_getSlotParams(__VLS_thisSlot);
    const __VLS_60 = {}.ElButton;
    /** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
    // @ts-ignore
    const __VLS_61 = __VLS_asFunctionalComponent(__VLS_60, new __VLS_60({
        ...{ 'onClick': {} },
        size: "small",
    }));
    const __VLS_62 = __VLS_61({
        ...{ 'onClick': {} },
        size: "small",
    }, ...__VLS_functionalComponentArgsRest(__VLS_61));
    let __VLS_64;
    let __VLS_65;
    let __VLS_66;
    const __VLS_67 = {
        onClick: (...[$event]) => {
            __VLS_ctx.openEditStore(row);
        }
    };
    __VLS_63.slots.default;
    var __VLS_63;
    const __VLS_68 = {}.ElButton;
    /** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
    // @ts-ignore
    const __VLS_69 = __VLS_asFunctionalComponent(__VLS_68, new __VLS_68({
        ...{ 'onClick': {} },
        size: "small",
    }));
    const __VLS_70 = __VLS_69({
        ...{ 'onClick': {} },
        size: "small",
    }, ...__VLS_functionalComponentArgsRest(__VLS_69));
    let __VLS_72;
    let __VLS_73;
    let __VLS_74;
    const __VLS_75 = {
        onClick: (...[$event]) => {
            __VLS_ctx.filterStorerooms(row.storeNum);
        }
    };
    __VLS_71.slots.default;
    var __VLS_71;
    const __VLS_76 = {}.ElButton;
    /** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
    // @ts-ignore
    const __VLS_77 = __VLS_asFunctionalComponent(__VLS_76, new __VLS_76({
        ...{ 'onClick': {} },
        size: "small",
        type: "danger",
        plain: true,
    }));
    const __VLS_78 = __VLS_77({
        ...{ 'onClick': {} },
        size: "small",
        type: "danger",
        plain: true,
    }, ...__VLS_functionalComponentArgsRest(__VLS_77));
    let __VLS_80;
    let __VLS_81;
    let __VLS_82;
    const __VLS_83 = {
        onClick: (...[$event]) => {
            __VLS_ctx.deleteStore(row.id);
        }
    };
    __VLS_79.slots.default;
    var __VLS_79;
}
var __VLS_59;
var __VLS_31;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ style: {} },
});
const __VLS_84 = {}.ElPagination;
/** @type {[typeof __VLS_components.ElPagination, typeof __VLS_components.elPagination, ]} */ ;
// @ts-ignore
const __VLS_85 = __VLS_asFunctionalComponent(__VLS_84, new __VLS_84({
    ...{ 'onChange': {} },
    background: true,
    layout: "prev, pager, next, sizes, total",
    total: (__VLS_ctx.storesTotal),
    currentPage: (__VLS_ctx.storesPage),
    pageSize: (__VLS_ctx.storesPageSize),
}));
const __VLS_86 = __VLS_85({
    ...{ 'onChange': {} },
    background: true,
    layout: "prev, pager, next, sizes, total",
    total: (__VLS_ctx.storesTotal),
    currentPage: (__VLS_ctx.storesPage),
    pageSize: (__VLS_ctx.storesPageSize),
}, ...__VLS_functionalComponentArgsRest(__VLS_85));
let __VLS_88;
let __VLS_89;
let __VLS_90;
const __VLS_91 = {
    onChange: (__VLS_ctx.loadStores)
};
var __VLS_87;
var __VLS_27;
const __VLS_92 = {}.ElTabPane;
/** @type {[typeof __VLS_components.ElTabPane, typeof __VLS_components.elTabPane, typeof __VLS_components.ElTabPane, typeof __VLS_components.elTabPane, ]} */ ;
// @ts-ignore
const __VLS_93 = __VLS_asFunctionalComponent(__VLS_92, new __VLS_92({
    label: "库房",
    name: "storeroom",
}));
const __VLS_94 = __VLS_93({
    label: "库房",
    name: "storeroom",
}, ...__VLS_functionalComponentArgsRest(__VLS_93));
__VLS_95.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ style: {} },
});
const __VLS_96 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_97 = __VLS_asFunctionalComponent(__VLS_96, new __VLS_96({
    modelValue: (__VLS_ctx.storeroomStoreNum),
    placeholder: "仓库编码（筛选）",
    ...{ style: {} },
    clearable: true,
}));
const __VLS_98 = __VLS_97({
    modelValue: (__VLS_ctx.storeroomStoreNum),
    placeholder: "仓库编码（筛选）",
    ...{ style: {} },
    clearable: true,
}, ...__VLS_functionalComponentArgsRest(__VLS_97));
const __VLS_100 = {}.ElButton;
/** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
// @ts-ignore
const __VLS_101 = __VLS_asFunctionalComponent(__VLS_100, new __VLS_100({
    ...{ 'onClick': {} },
    type: "primary",
}));
const __VLS_102 = __VLS_101({
    ...{ 'onClick': {} },
    type: "primary",
}, ...__VLS_functionalComponentArgsRest(__VLS_101));
let __VLS_104;
let __VLS_105;
let __VLS_106;
const __VLS_107 = {
    onClick: (__VLS_ctx.openCreateStoreroom)
};
__VLS_103.slots.default;
var __VLS_103;
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
    onClick: (__VLS_ctx.loadStorerooms)
};
__VLS_111.slots.default;
var __VLS_111;
const __VLS_116 = {}.ElTable;
/** @type {[typeof __VLS_components.ElTable, typeof __VLS_components.elTable, typeof __VLS_components.ElTable, typeof __VLS_components.elTable, ]} */ ;
// @ts-ignore
const __VLS_117 = __VLS_asFunctionalComponent(__VLS_116, new __VLS_116({
    data: (__VLS_ctx.storerooms),
    ...{ style: {} },
}));
const __VLS_118 = __VLS_117({
    data: (__VLS_ctx.storerooms),
    ...{ style: {} },
}, ...__VLS_functionalComponentArgsRest(__VLS_117));
__VLS_asFunctionalDirective(__VLS_directives.vLoading)(null, { ...__VLS_directiveBindingRestFields, value: (__VLS_ctx.loadingStorerooms) }, null, null);
__VLS_119.slots.default;
const __VLS_120 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_121 = __VLS_asFunctionalComponent(__VLS_120, new __VLS_120({
    prop: "storeNum",
    label: "仓库编码",
    minWidth: "140",
}));
const __VLS_122 = __VLS_121({
    prop: "storeNum",
    label: "仓库编码",
    minWidth: "140",
}, ...__VLS_functionalComponentArgsRest(__VLS_121));
const __VLS_124 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_125 = __VLS_asFunctionalComponent(__VLS_124, new __VLS_124({
    prop: "storeroomNum",
    label: "库房编码",
    minWidth: "140",
}));
const __VLS_126 = __VLS_125({
    prop: "storeroomNum",
    label: "库房编码",
    minWidth: "140",
}, ...__VLS_functionalComponentArgsRest(__VLS_125));
const __VLS_128 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_129 = __VLS_asFunctionalComponent(__VLS_128, new __VLS_128({
    prop: "storeroomName",
    label: "库房名称",
    minWidth: "180",
}));
const __VLS_130 = __VLS_129({
    prop: "storeroomName",
    label: "库房名称",
    minWidth: "180",
}, ...__VLS_functionalComponentArgsRest(__VLS_129));
const __VLS_132 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_133 = __VLS_asFunctionalComponent(__VLS_132, new __VLS_132({
    prop: "dangerLevel",
    label: "危险等级",
    width: "110",
}));
const __VLS_134 = __VLS_133({
    prop: "dangerLevel",
    label: "危险等级",
    width: "110",
}, ...__VLS_functionalComponentArgsRest(__VLS_133));
const __VLS_136 = {}.ElTableColumn;
/** @type {[typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, typeof __VLS_components.ElTableColumn, typeof __VLS_components.elTableColumn, ]} */ ;
// @ts-ignore
const __VLS_137 = __VLS_asFunctionalComponent(__VLS_136, new __VLS_136({
    label: "操作",
    width: "160",
    fixed: "right",
}));
const __VLS_138 = __VLS_137({
    label: "操作",
    width: "160",
    fixed: "right",
}, ...__VLS_functionalComponentArgsRest(__VLS_137));
__VLS_139.slots.default;
{
    const { default: __VLS_thisSlot } = __VLS_139.slots;
    const [{ row }] = __VLS_getSlotParams(__VLS_thisSlot);
    const __VLS_140 = {}.ElButton;
    /** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
    // @ts-ignore
    const __VLS_141 = __VLS_asFunctionalComponent(__VLS_140, new __VLS_140({
        ...{ 'onClick': {} },
        size: "small",
    }));
    const __VLS_142 = __VLS_141({
        ...{ 'onClick': {} },
        size: "small",
    }, ...__VLS_functionalComponentArgsRest(__VLS_141));
    let __VLS_144;
    let __VLS_145;
    let __VLS_146;
    const __VLS_147 = {
        onClick: (...[$event]) => {
            __VLS_ctx.openEditStoreroom(row);
        }
    };
    __VLS_143.slots.default;
    var __VLS_143;
    const __VLS_148 = {}.ElButton;
    /** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
    // @ts-ignore
    const __VLS_149 = __VLS_asFunctionalComponent(__VLS_148, new __VLS_148({
        ...{ 'onClick': {} },
        size: "small",
        type: "danger",
        plain: true,
    }));
    const __VLS_150 = __VLS_149({
        ...{ 'onClick': {} },
        size: "small",
        type: "danger",
        plain: true,
    }, ...__VLS_functionalComponentArgsRest(__VLS_149));
    let __VLS_152;
    let __VLS_153;
    let __VLS_154;
    const __VLS_155 = {
        onClick: (...[$event]) => {
            __VLS_ctx.deleteStoreroom(row.id);
        }
    };
    __VLS_151.slots.default;
    var __VLS_151;
}
var __VLS_139;
var __VLS_119;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ style: {} },
});
const __VLS_156 = {}.ElPagination;
/** @type {[typeof __VLS_components.ElPagination, typeof __VLS_components.elPagination, ]} */ ;
// @ts-ignore
const __VLS_157 = __VLS_asFunctionalComponent(__VLS_156, new __VLS_156({
    ...{ 'onChange': {} },
    background: true,
    layout: "prev, pager, next, sizes, total",
    total: (__VLS_ctx.storeroomsTotal),
    currentPage: (__VLS_ctx.storeroomsPage),
    pageSize: (__VLS_ctx.storeroomsPageSize),
}));
const __VLS_158 = __VLS_157({
    ...{ 'onChange': {} },
    background: true,
    layout: "prev, pager, next, sizes, total",
    total: (__VLS_ctx.storeroomsTotal),
    currentPage: (__VLS_ctx.storeroomsPage),
    pageSize: (__VLS_ctx.storeroomsPageSize),
}, ...__VLS_functionalComponentArgsRest(__VLS_157));
let __VLS_160;
let __VLS_161;
let __VLS_162;
const __VLS_163 = {
    onChange: (__VLS_ctx.loadStorerooms)
};
var __VLS_159;
var __VLS_95;
var __VLS_23;
const __VLS_164 = {}.ElDialog;
/** @type {[typeof __VLS_components.ElDialog, typeof __VLS_components.elDialog, typeof __VLS_components.ElDialog, typeof __VLS_components.elDialog, ]} */ ;
// @ts-ignore
const __VLS_165 = __VLS_asFunctionalComponent(__VLS_164, new __VLS_164({
    modelValue: (__VLS_ctx.storeDialog),
    title: (__VLS_ctx.storeEditing?.id ? '编辑仓库' : '新增仓库'),
    width: "640px",
}));
const __VLS_166 = __VLS_165({
    modelValue: (__VLS_ctx.storeDialog),
    title: (__VLS_ctx.storeEditing?.id ? '编辑仓库' : '新增仓库'),
    width: "640px",
}, ...__VLS_functionalComponentArgsRest(__VLS_165));
__VLS_167.slots.default;
const __VLS_168 = {}.ElForm;
/** @type {[typeof __VLS_components.ElForm, typeof __VLS_components.elForm, typeof __VLS_components.ElForm, typeof __VLS_components.elForm, ]} */ ;
// @ts-ignore
const __VLS_169 = __VLS_asFunctionalComponent(__VLS_168, new __VLS_168({
    labelWidth: "100px",
}));
const __VLS_170 = __VLS_169({
    labelWidth: "100px",
}, ...__VLS_functionalComponentArgsRest(__VLS_169));
__VLS_171.slots.default;
const __VLS_172 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_173 = __VLS_asFunctionalComponent(__VLS_172, new __VLS_172({
    label: "企业编码",
    required: true,
}));
const __VLS_174 = __VLS_173({
    label: "企业编码",
    required: true,
}, ...__VLS_functionalComponentArgsRest(__VLS_173));
__VLS_175.slots.default;
const __VLS_176 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_177 = __VLS_asFunctionalComponent(__VLS_176, new __VLS_176({
    modelValue: (__VLS_ctx.storeForm.companyCode),
}));
const __VLS_178 = __VLS_177({
    modelValue: (__VLS_ctx.storeForm.companyCode),
}, ...__VLS_functionalComponentArgsRest(__VLS_177));
var __VLS_175;
const __VLS_180 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_181 = __VLS_asFunctionalComponent(__VLS_180, new __VLS_180({
    label: "仓库编码",
    required: true,
}));
const __VLS_182 = __VLS_181({
    label: "仓库编码",
    required: true,
}, ...__VLS_functionalComponentArgsRest(__VLS_181));
__VLS_183.slots.default;
const __VLS_184 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_185 = __VLS_asFunctionalComponent(__VLS_184, new __VLS_184({
    modelValue: (__VLS_ctx.storeForm.storeNum),
}));
const __VLS_186 = __VLS_185({
    modelValue: (__VLS_ctx.storeForm.storeNum),
}, ...__VLS_functionalComponentArgsRest(__VLS_185));
var __VLS_183;
const __VLS_188 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_189 = __VLS_asFunctionalComponent(__VLS_188, new __VLS_188({
    label: "仓库名称",
    required: true,
}));
const __VLS_190 = __VLS_189({
    label: "仓库名称",
    required: true,
}, ...__VLS_functionalComponentArgsRest(__VLS_189));
__VLS_191.slots.default;
const __VLS_192 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_193 = __VLS_asFunctionalComponent(__VLS_192, new __VLS_192({
    modelValue: (__VLS_ctx.storeForm.storeName),
}));
const __VLS_194 = __VLS_193({
    modelValue: (__VLS_ctx.storeForm.storeName),
}, ...__VLS_functionalComponentArgsRest(__VLS_193));
var __VLS_191;
const __VLS_196 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_197 = __VLS_asFunctionalComponent(__VLS_196, new __VLS_196({
    label: "危险等级",
}));
const __VLS_198 = __VLS_197({
    label: "危险等级",
}, ...__VLS_functionalComponentArgsRest(__VLS_197));
__VLS_199.slots.default;
const __VLS_200 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_201 = __VLS_asFunctionalComponent(__VLS_200, new __VLS_200({
    modelValue: (__VLS_ctx.storeForm.dangerLevel),
    placeholder: "01/02/03",
}));
const __VLS_202 = __VLS_201({
    modelValue: (__VLS_ctx.storeForm.dangerLevel),
    placeholder: "01/02/03",
}, ...__VLS_functionalComponentArgsRest(__VLS_201));
var __VLS_199;
const __VLS_204 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_205 = __VLS_asFunctionalComponent(__VLS_204, new __VLS_204({
    label: "核定药量",
}));
const __VLS_206 = __VLS_205({
    label: "核定药量",
}, ...__VLS_functionalComponentArgsRest(__VLS_205));
__VLS_207.slots.default;
const __VLS_208 = {}.ElInputNumber;
/** @type {[typeof __VLS_components.ElInputNumber, typeof __VLS_components.elInputNumber, ]} */ ;
// @ts-ignore
const __VLS_209 = __VLS_asFunctionalComponent(__VLS_208, new __VLS_208({
    modelValue: (__VLS_ctx.storeForm.quotaDosage),
    ...{ style: {} },
}));
const __VLS_210 = __VLS_209({
    modelValue: (__VLS_ctx.storeForm.quotaDosage),
    ...{ style: {} },
}, ...__VLS_functionalComponentArgsRest(__VLS_209));
var __VLS_207;
const __VLS_212 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_213 = __VLS_asFunctionalComponent(__VLS_212, new __VLS_212({
    label: "核定人数",
}));
const __VLS_214 = __VLS_213({
    label: "核定人数",
}, ...__VLS_functionalComponentArgsRest(__VLS_213));
__VLS_215.slots.default;
const __VLS_216 = {}.ElInputNumber;
/** @type {[typeof __VLS_components.ElInputNumber, typeof __VLS_components.elInputNumber, ]} */ ;
// @ts-ignore
const __VLS_217 = __VLS_asFunctionalComponent(__VLS_216, new __VLS_216({
    modelValue: (__VLS_ctx.storeForm.quotaPeople),
    ...{ style: {} },
}));
const __VLS_218 = __VLS_217({
    modelValue: (__VLS_ctx.storeForm.quotaPeople),
    ...{ style: {} },
}, ...__VLS_functionalComponentArgsRest(__VLS_217));
var __VLS_215;
var __VLS_171;
{
    const { footer: __VLS_thisSlot } = __VLS_167.slots;
    const __VLS_220 = {}.ElButton;
    /** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
    // @ts-ignore
    const __VLS_221 = __VLS_asFunctionalComponent(__VLS_220, new __VLS_220({
        ...{ 'onClick': {} },
    }));
    const __VLS_222 = __VLS_221({
        ...{ 'onClick': {} },
    }, ...__VLS_functionalComponentArgsRest(__VLS_221));
    let __VLS_224;
    let __VLS_225;
    let __VLS_226;
    const __VLS_227 = {
        onClick: (...[$event]) => {
            __VLS_ctx.storeDialog = false;
        }
    };
    __VLS_223.slots.default;
    var __VLS_223;
    const __VLS_228 = {}.ElButton;
    /** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
    // @ts-ignore
    const __VLS_229 = __VLS_asFunctionalComponent(__VLS_228, new __VLS_228({
        ...{ 'onClick': {} },
        type: "primary",
        loading: (__VLS_ctx.savingStore),
    }));
    const __VLS_230 = __VLS_229({
        ...{ 'onClick': {} },
        type: "primary",
        loading: (__VLS_ctx.savingStore),
    }, ...__VLS_functionalComponentArgsRest(__VLS_229));
    let __VLS_232;
    let __VLS_233;
    let __VLS_234;
    const __VLS_235 = {
        onClick: (__VLS_ctx.saveStore)
    };
    __VLS_231.slots.default;
    var __VLS_231;
}
var __VLS_167;
const __VLS_236 = {}.ElDialog;
/** @type {[typeof __VLS_components.ElDialog, typeof __VLS_components.elDialog, typeof __VLS_components.ElDialog, typeof __VLS_components.elDialog, ]} */ ;
// @ts-ignore
const __VLS_237 = __VLS_asFunctionalComponent(__VLS_236, new __VLS_236({
    modelValue: (__VLS_ctx.storeroomDialog),
    title: (__VLS_ctx.storeroomEditing?.id ? '编辑库房' : '新增库房'),
    width: "640px",
}));
const __VLS_238 = __VLS_237({
    modelValue: (__VLS_ctx.storeroomDialog),
    title: (__VLS_ctx.storeroomEditing?.id ? '编辑库房' : '新增库房'),
    width: "640px",
}, ...__VLS_functionalComponentArgsRest(__VLS_237));
__VLS_239.slots.default;
const __VLS_240 = {}.ElForm;
/** @type {[typeof __VLS_components.ElForm, typeof __VLS_components.elForm, typeof __VLS_components.ElForm, typeof __VLS_components.elForm, ]} */ ;
// @ts-ignore
const __VLS_241 = __VLS_asFunctionalComponent(__VLS_240, new __VLS_240({
    labelWidth: "100px",
}));
const __VLS_242 = __VLS_241({
    labelWidth: "100px",
}, ...__VLS_functionalComponentArgsRest(__VLS_241));
__VLS_243.slots.default;
const __VLS_244 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_245 = __VLS_asFunctionalComponent(__VLS_244, new __VLS_244({
    label: "仓库编码",
    required: true,
}));
const __VLS_246 = __VLS_245({
    label: "仓库编码",
    required: true,
}, ...__VLS_functionalComponentArgsRest(__VLS_245));
__VLS_247.slots.default;
const __VLS_248 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_249 = __VLS_asFunctionalComponent(__VLS_248, new __VLS_248({
    modelValue: (__VLS_ctx.storeroomForm.storeNum),
}));
const __VLS_250 = __VLS_249({
    modelValue: (__VLS_ctx.storeroomForm.storeNum),
}, ...__VLS_functionalComponentArgsRest(__VLS_249));
var __VLS_247;
const __VLS_252 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_253 = __VLS_asFunctionalComponent(__VLS_252, new __VLS_252({
    label: "库房编码",
    required: true,
}));
const __VLS_254 = __VLS_253({
    label: "库房编码",
    required: true,
}, ...__VLS_functionalComponentArgsRest(__VLS_253));
__VLS_255.slots.default;
const __VLS_256 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_257 = __VLS_asFunctionalComponent(__VLS_256, new __VLS_256({
    modelValue: (__VLS_ctx.storeroomForm.storeroomNum),
}));
const __VLS_258 = __VLS_257({
    modelValue: (__VLS_ctx.storeroomForm.storeroomNum),
}, ...__VLS_functionalComponentArgsRest(__VLS_257));
var __VLS_255;
const __VLS_260 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_261 = __VLS_asFunctionalComponent(__VLS_260, new __VLS_260({
    label: "库房名称",
    required: true,
}));
const __VLS_262 = __VLS_261({
    label: "库房名称",
    required: true,
}, ...__VLS_functionalComponentArgsRest(__VLS_261));
__VLS_263.slots.default;
const __VLS_264 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_265 = __VLS_asFunctionalComponent(__VLS_264, new __VLS_264({
    modelValue: (__VLS_ctx.storeroomForm.storeroomName),
}));
const __VLS_266 = __VLS_265({
    modelValue: (__VLS_ctx.storeroomForm.storeroomName),
}, ...__VLS_functionalComponentArgsRest(__VLS_265));
var __VLS_263;
const __VLS_268 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_269 = __VLS_asFunctionalComponent(__VLS_268, new __VLS_268({
    label: "危险等级",
}));
const __VLS_270 = __VLS_269({
    label: "危险等级",
}, ...__VLS_functionalComponentArgsRest(__VLS_269));
__VLS_271.slots.default;
const __VLS_272 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_273 = __VLS_asFunctionalComponent(__VLS_272, new __VLS_272({
    modelValue: (__VLS_ctx.storeroomForm.dangerLevel),
    placeholder: "01/02/03",
}));
const __VLS_274 = __VLS_273({
    modelValue: (__VLS_ctx.storeroomForm.dangerLevel),
    placeholder: "01/02/03",
}, ...__VLS_functionalComponentArgsRest(__VLS_273));
var __VLS_271;
const __VLS_276 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_277 = __VLS_asFunctionalComponent(__VLS_276, new __VLS_276({
    label: "核定药量",
}));
const __VLS_278 = __VLS_277({
    label: "核定药量",
}, ...__VLS_functionalComponentArgsRest(__VLS_277));
__VLS_279.slots.default;
const __VLS_280 = {}.ElInputNumber;
/** @type {[typeof __VLS_components.ElInputNumber, typeof __VLS_components.elInputNumber, ]} */ ;
// @ts-ignore
const __VLS_281 = __VLS_asFunctionalComponent(__VLS_280, new __VLS_280({
    modelValue: (__VLS_ctx.storeroomForm.quotaDosage),
    ...{ style: {} },
}));
const __VLS_282 = __VLS_281({
    modelValue: (__VLS_ctx.storeroomForm.quotaDosage),
    ...{ style: {} },
}, ...__VLS_functionalComponentArgsRest(__VLS_281));
var __VLS_279;
const __VLS_284 = {}.ElFormItem;
/** @type {[typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, typeof __VLS_components.ElFormItem, typeof __VLS_components.elFormItem, ]} */ ;
// @ts-ignore
const __VLS_285 = __VLS_asFunctionalComponent(__VLS_284, new __VLS_284({
    label: "核定人数",
}));
const __VLS_286 = __VLS_285({
    label: "核定人数",
}, ...__VLS_functionalComponentArgsRest(__VLS_285));
__VLS_287.slots.default;
const __VLS_288 = {}.ElInputNumber;
/** @type {[typeof __VLS_components.ElInputNumber, typeof __VLS_components.elInputNumber, ]} */ ;
// @ts-ignore
const __VLS_289 = __VLS_asFunctionalComponent(__VLS_288, new __VLS_288({
    modelValue: (__VLS_ctx.storeroomForm.quotaPeople),
    ...{ style: {} },
}));
const __VLS_290 = __VLS_289({
    modelValue: (__VLS_ctx.storeroomForm.quotaPeople),
    ...{ style: {} },
}, ...__VLS_functionalComponentArgsRest(__VLS_289));
var __VLS_287;
var __VLS_243;
{
    const { footer: __VLS_thisSlot } = __VLS_239.slots;
    const __VLS_292 = {}.ElButton;
    /** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
    // @ts-ignore
    const __VLS_293 = __VLS_asFunctionalComponent(__VLS_292, new __VLS_292({
        ...{ 'onClick': {} },
    }));
    const __VLS_294 = __VLS_293({
        ...{ 'onClick': {} },
    }, ...__VLS_functionalComponentArgsRest(__VLS_293));
    let __VLS_296;
    let __VLS_297;
    let __VLS_298;
    const __VLS_299 = {
        onClick: (...[$event]) => {
            __VLS_ctx.storeroomDialog = false;
        }
    };
    __VLS_295.slots.default;
    var __VLS_295;
    const __VLS_300 = {}.ElButton;
    /** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
    // @ts-ignore
    const __VLS_301 = __VLS_asFunctionalComponent(__VLS_300, new __VLS_300({
        ...{ 'onClick': {} },
        type: "primary",
        loading: (__VLS_ctx.savingStoreroom),
    }));
    const __VLS_302 = __VLS_301({
        ...{ 'onClick': {} },
        type: "primary",
        loading: (__VLS_ctx.savingStoreroom),
    }, ...__VLS_functionalComponentArgsRest(__VLS_301));
    let __VLS_304;
    let __VLS_305;
    let __VLS_306;
    const __VLS_307 = {
        onClick: (__VLS_ctx.saveStoreroom)
    };
    __VLS_303.slots.default;
    var __VLS_303;
}
var __VLS_239;
/** @type {__VLS_StyleScopedClasses['sf-title']} */ ;
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
        };
    },
});
export default (await import('vue')).defineComponent({
    setup() {
        return {};
    },
});
; /* PartiallyEnd: #4569/main.vue */
