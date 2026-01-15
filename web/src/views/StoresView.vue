<template>
  <div class="sf-page">
    <div class="sf-page-head">
      <div>
        <h2 class="sf-page-title">仓库库房</h2>
        <div class="sf-page-sub">仓库、库房与核定配置</div>
      </div>
      <div class="sf-page-actions">
        <span class="sf-badge">仓库数量 {{ storesTotal }}</span>
        <span class="sf-badge">库房数量 {{ storeroomsTotal }}</span>
        <el-input v-model="keyword" placeholder="编码/名称/企业编码" style="width: 240px" clearable />
        <el-button type="primary" @click="openCreateStore">新增仓库</el-button>
        <el-upload :show-file-list="false" :before-upload="importStores" accept=".csv">
          <el-button>导入仓库</el-button>
        </el-upload>
        <el-button @click="loadStores">刷新</el-button>
      </div>
    </div>

    <div class="sf-card sf-table-card">
      <el-tabs v-model="tab">
      <el-tab-pane label="仓库" name="store">
        <el-table :data="stores" v-loading="loadingStores" style="width: 100%">
          <el-table-column prop="companyCode" label="企业编码" min-width="140" />
          <el-table-column prop="storeNum" label="仓库编码" min-width="140" />
          <el-table-column prop="storeName" label="仓库名称" min-width="180" />
          <el-table-column prop="area" label="仓库面积（m²）" width="140" />
          <el-table-column prop="dangerLevel" label="危险等级" width="110" />
          <el-table-column prop="quotaDosage" label="核定药量（kg）" width="130" />
          <el-table-column prop="quotaPeople" label="核定人数（人）" width="130" />
          <el-table-column label="操作" width="200" fixed="right">
            <template #default="{ row }">
              <el-button size="small" @click="openEditStore(row)">编辑</el-button>
              <el-button size="small" @click="filterStorerooms(row.storeNum)">查看库房</el-button>
              <el-button size="small" type="danger" plain @click="deleteStore(row.id)">删除</el-button>
            </template>
          </el-table-column>
        </el-table>
        <div style="display: flex; justify-content: flex-end; margin-top: 12px">
          <el-pagination
            background
            layout="prev, pager, next, sizes, total"
            :total="storesTotal"
            v-model:current-page="storesPage"
            v-model:page-size="storesPageSize"
            @change="loadStores"
          />
        </div>
      </el-tab-pane>

      <el-tab-pane label="库房" name="storeroom">
        <div class="sf-filter" style="margin-bottom: 10px">
          <el-input v-model="storeroomStoreNum" placeholder="仓库编码（筛选）" style="width: 200px" clearable />
          <el-button type="primary" @click="openCreateStoreroom">新增库房</el-button>
          <el-upload :show-file-list="false" :before-upload="importStorerooms" accept=".csv">
            <el-button>导入库房</el-button>
          </el-upload>
          <el-button @click="loadStorerooms">刷新</el-button>
        </div>
        <el-table :data="storerooms" v-loading="loadingStorerooms" style="width: 100%">
          <el-table-column prop="storeNum" label="仓库编码" min-width="140" />
          <el-table-column prop="storeroomNum" label="库房编码" min-width="140" />
          <el-table-column prop="storeroomName" label="库房名称" min-width="180" />
          <el-table-column prop="area" label="库房面积（m²）" width="140" />
          <el-table-column prop="dangerLevel" label="危险等级" width="110" />
          <el-table-column prop="quotaDosage" label="核定药量（t）" width="120" />
          <el-table-column prop="quotaPeople" label="核定人数（人）" width="130" />
          <el-table-column label="操作" width="160" fixed="right">
            <template #default="{ row }">
              <el-button size="small" @click="openEditStoreroom(row)">编辑</el-button>
              <el-button size="small" type="danger" plain @click="deleteStoreroom(row.id)">删除</el-button>
            </template>
          </el-table-column>
        </el-table>
        <div style="display: flex; justify-content: flex-end; margin-top: 12px">
          <el-pagination
            background
            layout="prev, pager, next, sizes, total"
            :total="storeroomsTotal"
            v-model:current-page="storeroomsPage"
            v-model:page-size="storeroomsPageSize"
            @change="loadStorerooms"
          />
        </div>
      </el-tab-pane>
      </el-tabs>
    </div>

    <el-dialog v-model="storeDialog" :title="storeEditing?.id ? '编辑仓库' : '新增仓库'" width="640px">
      <el-form label-width="100px">
        <div class="sf-form-grid">
          <el-form-item label="企业编码" required><el-input v-model="storeForm.companyCode" /></el-form-item>
          <el-form-item label="仓库编码" required><el-input v-model="storeForm.storeNum" /></el-form-item>
          <el-form-item label="仓库名称" required><el-input v-model="storeForm.storeName" /></el-form-item>
          <el-form-item label="仓库面积（m²）"><el-input-number v-model="storeForm.area" style="width: 100%" /></el-form-item>
          <el-form-item label="危险等级"><el-input v-model="storeForm.dangerLevel" placeholder="01/02/03" /></el-form-item>
          <el-form-item label="核定药量（kg）"><el-input-number v-model="storeForm.quotaDosage" style="width: 100%" /></el-form-item>
          <el-form-item label="核定人数（人）"><el-input-number v-model="storeForm.quotaPeople" style="width: 100%" /></el-form-item>
        </div>
      </el-form>
      <template #footer>
        <el-button @click="storeDialog = false">取消</el-button>
        <el-button type="primary" :loading="savingStore" @click="saveStore">保存</el-button>
      </template>
    </el-dialog>

    <el-dialog v-model="storeroomDialog" :title="storeroomEditing?.id ? '编辑库房' : '新增库房'" width="640px">
      <el-form label-width="100px">
        <div class="sf-form-grid">
          <el-form-item label="仓库编码" required><el-input v-model="storeroomForm.storeNum" /></el-form-item>
          <el-form-item label="库房编码" required><el-input v-model="storeroomForm.storeroomNum" /></el-form-item>
          <el-form-item label="库房名称" required><el-input v-model="storeroomForm.storeroomName" /></el-form-item>
          <el-form-item label="库房面积（m²）"><el-input-number v-model="storeroomForm.area" style="width: 100%" /></el-form-item>
          <el-form-item label="危险等级"><el-input v-model="storeroomForm.dangerLevel" placeholder="01/02/03" /></el-form-item>
          <el-form-item label="核定药量（t）"><el-input-number v-model="storeroomForm.quotaDosage" style="width: 100%" /></el-form-item>
          <el-form-item label="核定人数（人）"><el-input-number v-model="storeroomForm.quotaPeople" style="width: 100%" /></el-form-item>
        </div>
      </el-form>
      <template #footer>
        <el-button @click="storeroomDialog = false">取消</el-button>
        <el-button type="primary" :loading="savingStoreroom" @click="saveStoreroom">保存</el-button>
      </template>
    </el-dialog>
  </div>
</template>

<script setup lang="ts">
import { reactive, ref, watch } from "vue";
import { ElMessage, ElMessageBox } from "element-plus";
import { http, rawHttp } from "@/api/http";

type PageResponse<T> = { list: T[]; page: number; pageSize: number; total: number };

type StoreVO = {
  id: number;
  companyCode: string;
  storeNum: string;
  storeName: string;
  area: number | null;
  dangerLevel: string | null;
  quotaDosage: number | null;
  quotaPeople: number | null;
};

type StoreroomVO = {
  id: number;
  storeNum: string;
  storeroomNum: string;
  storeroomName: string;
  area: number | null;
  dangerLevel: string | null;
  quotaDosage: number | null;
  quotaPeople: number | null;
};

const tab = ref<"store" | "storeroom">("store");
const keyword = ref("");

const loadingStores = ref(false);
const stores = ref<StoreVO[]>([]);
const storesTotal = ref(0);
const storesPage = ref(1);
const storesPageSize = ref(20);

const storeroomStoreNum = ref("");
const loadingStorerooms = ref(false);
const storerooms = ref<StoreroomVO[]>([]);
const storeroomsTotal = ref(0);
const storeroomsPage = ref(1);
const storeroomsPageSize = ref(20);

const storeDialog = ref(false);
const savingStore = ref(false);
const storeEditing = ref<StoreVO | null>(null);
const storeForm = reactive({
  companyCode: "",
  storeNum: "",
  storeName: "",
  area: null as number | null,
  dangerLevel: "",
  quotaDosage: null as number | null,
  quotaPeople: null as number | null
});

const storeroomDialog = ref(false);
const savingStoreroom = ref(false);
const storeroomEditing = ref<StoreroomVO | null>(null);
const storeroomForm = reactive({
  storeNum: "",
  storeroomNum: "",
  storeroomName: "",
  area: null as number | null,
  dangerLevel: "",
  quotaDosage: null as number | null,
  quotaPeople: null as number | null
});

async function loadStores() {
  loadingStores.value = true;
  try {
    const data = await http.get<PageResponse<StoreVO>>(
      `/api/v1/stores?page=${storesPage.value}&pageSize=${storesPageSize.value}${keyword.value ? `&keyword=${encodeURIComponent(keyword.value)}` : ""}`
    );
    stores.value = data.list;
    storesTotal.value = data.total;
  } catch (e: any) {
    ElMessage.error(e?.message || "加载失败");
  } finally {
    loadingStores.value = false;
  }
}

async function loadStorerooms() {
  loadingStorerooms.value = true;
  try {
    const data = await http.get<PageResponse<StoreroomVO>>(
      `/api/v1/storerooms?page=${storeroomsPage.value}&pageSize=${storeroomsPageSize.value}${storeroomStoreNum.value ? `&storeNum=${encodeURIComponent(storeroomStoreNum.value)}` : ""}`
    );
    storerooms.value = data.list;
    storeroomsTotal.value = data.total;
  } catch (e: any) {
    ElMessage.error(e?.message || "加载失败");
  } finally {
    loadingStorerooms.value = false;
  }
}

function filterStorerooms(storeNum: string) {
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

function openEditStore(row: StoreVO) {
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
      await http.post<number>("/api/v1/stores", { ...storeForm });
    } else {
      await http.put<void>("/api/v1/stores", { id: storeEditing.value.id, ...storeForm });
    }
    ElMessage.success("保存成功");
    storeDialog.value = false;
    await loadStores();
  } catch (e: any) {
    ElMessage.error(e?.message || "保存失败");
  } finally {
    savingStore.value = false;
  }
}

async function deleteStore(id: number) {
  const ok = await ElMessageBox.confirm("确认删除该仓库？", "提示", { type: "warning" }).catch(() => null);
  if (!ok) return;
  try {
    await http.del<void>(`/api/v1/stores/${id}`);
    ElMessage.success("删除成功");
    await loadStores();
  } catch (e: any) {
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

function openEditStoreroom(row: StoreroomVO) {
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
      await http.post<number>("/api/v1/storerooms", { ...storeroomForm });
    } else {
      await http.put<void>("/api/v1/storerooms", { id: storeroomEditing.value.id, ...storeroomForm });
    }
    ElMessage.success("保存成功");
    storeroomDialog.value = false;
    await loadStorerooms();
  } catch (e: any) {
    ElMessage.error(e?.message || "保存失败");
  } finally {
    savingStoreroom.value = false;
  }
}

async function deleteStoreroom(id: number) {
  const ok = await ElMessageBox.confirm("确认删除该库房？", "提示", { type: "warning" }).catch(() => null);
  if (!ok) return;
  try {
    await http.del<void>(`/api/v1/storerooms/${id}`);
    ElMessage.success("删除成功");
    await loadStorerooms();
  } catch (e: any) {
    ElMessage.error(e?.message || "删除失败");
  }
}

async function importStores(file: File) {
  const fd = new FormData();
  fd.append("file", file);
  try {
    const res = await rawHttp.post("/api/v1/stores/import", fd, {
      headers: { "Content-Type": "multipart/form-data" }
    });
    ElMessage.success(`导入完成：成功 ${res.successCount} 条，失败 ${res.failCount} 条`);
    await loadStores();
  } catch (e: any) {
    ElMessage.error(e?.message || "导入失败");
  }
  return false;
}

async function importStorerooms(file: File) {
  const fd = new FormData();
  fd.append("file", file);
  try {
    const res = await rawHttp.post("/api/v1/storerooms/import", fd, {
      headers: { "Content-Type": "multipart/form-data" }
    });
    ElMessage.success(`导入完成：成功 ${res.successCount} 条，失败 ${res.failCount} 条`);
    await loadStorerooms();
  } catch (e: any) {
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
  if (tab.value === "storeroom") loadStorerooms();
});

loadStores();
</script>
