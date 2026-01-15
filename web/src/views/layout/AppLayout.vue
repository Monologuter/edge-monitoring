<template>
  <div class="layout">
    <aside class="sidebar sf-card sf-glow">
      <div class="brand">
        <div class="logo">
          <div class="logoDot"></div>
        </div>
        <div class="brandText">
          <div class="brandTitle">安全监测预警系统</div>
          <div class="brandSub">Safety Monitoring Platform</div>
        </div>
      </div>

      <div class="navTitle">系统导航</div>
      <nav class="nav sf-stagger" style="--sf-stagger: 60ms">
        <template v-for="m in menus" :key="m.id">
          <!-- 有子菜单的一级菜单 -->
          <div v-if="m.children && m.children.length > 0" class="menuGroup" style="--i: 40ms">
            <div
              class="navItem navItemParent"
              :class="{ active: isMenuActive(m) }"
              @click="toggleMenu(m.id)"
            >
              <span class="navIcon">
                <component :is="iconOf(m.icon)" />
              </span>
              <span class="navLabel">{{ m.menuName }}</span>
              <span class="arrow" :class="{ expanded: expandedMenus.includes(m.id) }">▼</span>
            </div>
            <div v-show="expandedMenus.includes(m.id)" class="subMenu">
              <RouterLink
                v-for="sub in m.children"
                :key="sub.id"
                class="subNavItem"
                :to="sub.path"
              >
                <span>{{ sub.menuName }}</span>
              </RouterLink>
            </div>
          </div>

          <!-- 无子菜单的菜单项 -->
          <RouterLink v-else class="navItem" :to="m.path" style="--i: 40ms">
            <span class="navIcon">
              <component :is="iconOf(m.icon)" />
            </span>
            <span>{{ m.menuName }}</span>
          </RouterLink>
        </template>
      </nav>

      <div class="sidebarActions">
        <el-button type="primary" plain size="small" class="actionBtn">新增项目</el-button>
        <el-button size="small" class="actionBtn">系统设置</el-button>
      </div>

      <div class="sidebarFoot sf-muted">
        <div>安全监测 · 风险预警 · 闭环处置</div>
        <div>版本 2026.01</div>
      </div>
    </aside>

    <main class="main">
      <header class="topbar sf-card sf-glow">
        <div class="pageInfo">
          <div class="pageTitle">{{ currentTitle }}</div>
          <div class="pageSub">风险监测 · 预警联动 · 闭环处置</div>
        </div>
        <div class="topActions">
          <el-input class="searchInput" placeholder="搜索今日报警事件">
            <template #prefix>
              <el-icon><Search /></el-icon>
            </template>
          </el-input>
          <el-button class="iconBtn" text circle>
            <el-icon><Bell /></el-icon>
          </el-button>
          <el-dropdown>
            <div class="user">
              <div class="avatar">{{ userInitial }}</div>
              <div class="name">{{ auth.me?.displayName || auth.me?.username || "管理员" }}</div>
              <el-icon class="arrowIcon"><ArrowDown /></el-icon>
            </div>
            <template #dropdown>
              <el-dropdown-menu>
                <el-dropdown-item>个人中心</el-dropdown-item>
                <el-dropdown-item divided @click="logout">退出登录</el-dropdown-item>
              </el-dropdown-menu>
            </template>
          </el-dropdown>
        </div>
      </header>

      <div class="content sf-page">
        <div class="pageFrame sf-panel sf-animate-in" :key="route.fullPath">
          <RouterView />
        </div>
      </div>
    </main>
  </div>
</template>

<script setup lang="ts">
import { useAuthStore } from "@/store/auth";
import router from "@/router";
import {
  Bell,
  Box,
  Connection,
  Cpu,
  DataLine,
  Document,
  Monitor,
  OfficeBuilding,
  User,
  Van,
  VideoCamera,
  Setting,
  Search,
  ArrowDown
} from "@element-plus/icons-vue";
import { computed, ref, watch } from "vue";
import { useRoute } from "vue-router";

const auth = useAuthStore();
const route = useRoute();

const menus = computed(() => auth.me?.menus || []);
const expandedMenus = ref<number[]>([]);

const iconMap: Record<string, any> = {
  Monitor,
  Bell,
  OfficeBuilding,
  User,
  Van,
  Box,
  Cpu,
  VideoCamera,
  Connection,
  Document,
  DataLine,
  Setting
};

const currentTitle = computed(() => {
  const currentPath = route.path;
  for (const menu of menus.value) {
    if (menu.path === currentPath) return menu.menuName;
    if (menu.children?.length) {
      const found = menu.children.find((sub: any) => sub.path === currentPath);
      if (found) return found.menuName;
    }
  }
  return "数据看板";
});

const userInitial = computed(() => {
  const name = auth.me?.displayName || auth.me?.username || "管";
  return name.slice(0, 1);
});

function iconOf(name?: string | null) {
  if (!name) return Monitor;
  return iconMap[name] || Monitor;
}

function toggleMenu(menuId: number) {
  const index = expandedMenus.value.indexOf(menuId);
  if (index === -1) {
    expandedMenus.value.push(menuId);
  } else {
    expandedMenus.value.splice(index, 1);
  }
}

function isMenuActive(menu: any): boolean {
  if (!menu.children) return false;
  const currentPath = route.path;
  return menu.children.some((sub: any) => sub.path === currentPath);
}

function syncExpandedMenus() {
  const currentPath = route.path;
  const parent = menus.value.find((menu) =>
    menu.children?.some((sub: any) => sub.path === currentPath)
  );
  if (!parent) return;
  if (!expandedMenus.value.includes(parent.id)) {
    expandedMenus.value.push(parent.id);
  }
}

watch(
  () => [menus.value, route.path],
  () => syncExpandedMenus(),
  { immediate: true }
);

function logout() {
  auth.logout();
  router.replace("/login");
}
</script>

<style scoped>
.layout {
  height: 100vh;
  display: grid;
  grid-template-columns: 252px 1fr;
  gap: 18px;
  padding: 20px 22px;
}

.sidebar {
  padding: 16px 14px;
  display: flex;
  flex-direction: column;
  background: var(--sf-panel);
  border-radius: var(--sf-radius-lg);
}

.brand {
  display: flex;
  gap: 12px;
  align-items: center;
  padding: 8px 8px 12px;
  border-bottom: 1px solid rgba(33, 54, 98, 0.08);
}

.logo {
  width: 38px;
  height: 38px;
  border-radius: 12px;
  background: radial-gradient(circle at 30% 30%, rgba(39, 183, 167, 0.9), transparent 55%),
    radial-gradient(circle at 75% 10%, rgba(47, 107, 255, 0.8), transparent 55%),
    rgba(47, 107, 255, 0.08);
  border: 1px solid rgba(47, 107, 255, 0.18);
  position: relative;
}

.logoDot {
  position: absolute;
  inset: 9px;
  border-radius: 10px;
  border: 1px dashed rgba(47, 107, 255, 0.3);
}

.brandTitle {
  font-weight: 700;
  font-size: 15px;
  letter-spacing: 0.4px;
}

.brandSub {
  margin-top: 2px;
  font-size: 12px;
  color: var(--sf-text-2);
}

.navTitle {
  margin-top: 12px;
  padding: 0 10px;
  font-size: 12px;
  color: var(--sf-text-2);
  letter-spacing: 0.4px;
}

.nav {
  padding: 14px 4px;
  display: grid;
  gap: 8px;
}

.menuGroup {
  display: flex;
  flex-direction: column;
  gap: 4px;
}

.navItem {
  text-decoration: none;
  color: var(--sf-text-1);
  padding: 10px 12px;
  border-radius: 12px;
  border: 1px solid transparent;
  transition: 160ms ease;
  background: rgba(47, 107, 255, 0.06);
  display: flex;
  align-items: center;
  gap: 10px;
  cursor: pointer;
}

.navItemParent {
  position: relative;
}

.navLabel {
  flex: 1;
}

.arrow {
  font-size: 10px;
  transition: transform 200ms ease;
  opacity: 0.6;
}

.arrow.expanded {
  transform: rotate(180deg);
}

.navIcon {
  width: 18px;
  height: 18px;
  display: inline-flex;
  align-items: center;
  justify-content: center;
  color: rgba(47, 107, 255, 0.75);
}

.navItem:hover {
  border-color: rgba(47, 107, 255, 0.25);
  color: var(--sf-text-0);
}

.router-link-active {
  background: linear-gradient(135deg, rgba(47, 107, 255, 0.2), rgba(39, 183, 167, 0.2));
  border-color: rgba(47, 107, 255, 0.3);
  color: var(--sf-text-0);
}

.navItemParent.active {
  background: linear-gradient(135deg, rgba(47, 107, 255, 0.2), rgba(39, 183, 167, 0.2));
  border-color: rgba(47, 107, 255, 0.3);
  color: var(--sf-text-0);
}

/* 二级菜单样式 */
.subMenu {
  display: flex;
  flex-direction: column;
  gap: 4px;
  padding-left: 20px;
  margin-top: 4px;
}

.subNavItem {
  text-decoration: none;
  color: var(--sf-text-0);
  padding: 8px 12px;
  border-radius: 10px;
  border: 1px solid rgba(33, 54, 98, 0.08);
  transition: 160ms ease;
  background: rgba(47, 107, 255, 0.08);
  font-size: 13px;
  display: flex;
  align-items: center;
}

.subNavItem:hover {
  background: rgba(47, 107, 255, 0.16);
  color: var(--sf-text-0);
  border-color: rgba(47, 107, 255, 0.3);
}

.subNavItem.router-link-active {
  background: rgba(47, 107, 255, 0.24);
  color: var(--sf-text-0);
  border-color: rgba(47, 107, 255, 0.45);
  font-weight: 600;
  box-shadow: 0 10px 20px rgba(47, 107, 255, 0.16);
}

.sidebarActions {
  margin-top: 10px;
  display: grid;
  gap: 8px;
  padding: 8px;
}

.actionBtn {
  justify-content: center;
}

.sidebarFoot {
  margin-top: auto;
  padding: 12px 8px 6px;
  border-top: 1px solid rgba(33, 54, 98, 0.08);
  font-size: 12px;
  display: grid;
  gap: 4px;
}

.main {
  min-width: 0;
  display: flex;
  flex-direction: column;
  gap: 16px;
}

.topbar {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 14px 18px;
  background: linear-gradient(120deg, #ffffff, #f5f7fb);
  border-radius: var(--sf-radius-lg);
  border: 1px solid var(--sf-card-border);
  box-shadow: var(--sf-shadow-soft);
}

.pageInfo {
  display: grid;
  gap: 6px;
}

.pageTitle {
  font-size: 18px;
  font-weight: 700;
  letter-spacing: 0.3px;
}

.pageSub {
  font-size: 12px;
  color: var(--sf-text-2);
}

.user {
  display: flex;
  align-items: center;
  gap: 10px;
  cursor: pointer;
}

.avatar {
  width: 34px;
  height: 34px;
  border-radius: 12px;
  display: grid;
  place-items: center;
  font-weight: 700;
  font-size: 13px;
  color: #ffffff;
  background: linear-gradient(135deg, #2f6bff, #27b7a7);
  box-shadow: 0 8px 18px rgba(47, 107, 255, 0.25);
}

.name {
  font-size: 13px;
  color: var(--sf-text-1);
}

.arrowIcon {
  font-size: 12px;
  color: var(--sf-text-2);
}

.topActions {
  display: flex;
  align-items: center;
  gap: 12px;
}

.searchInput {
  width: 240px;
}

.iconBtn {
  color: var(--sf-text-1);
  background: rgba(47, 107, 255, 0.08);
}

.content {
  min-height: 0;
  flex: 1;
  overflow: auto;
}

.pageFrame {
  padding: 16px;
}

@media (max-width: 1100px) {
  .layout {
    grid-template-columns: 1fr;
    padding: 14px;
  }

  .sidebar {
    flex-direction: column;
  }

  .nav {
    display: flex;
    overflow-x: auto;
    padding-bottom: 10px;
  }

  .menuGroup,
  .navItem {
    min-width: 180px;
  }

  .topbar {
    flex-direction: column;
    align-items: flex-start;
    gap: 12px;
  }

  .topActions {
    width: 100%;
    flex-wrap: wrap;
    justify-content: space-between;
  }

  .searchInput {
    width: 100%;
  }
}
</style>
