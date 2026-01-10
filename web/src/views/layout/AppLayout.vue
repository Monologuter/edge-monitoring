<template>
  <div class="layout">
    <aside class="sidebar sf-card">
      <div class="brand">
        <div class="logo">
          <div class="logoDot"></div>
        </div>
        <div class="brandText">
          <div class="brandTitle">安全监测预警系统</div>
          <div class="brandSub">Safety · Monitor · Alert</div>
        </div>
      </div>

      <nav class="nav">
        <template v-for="m in menus" :key="m.id">
          <!-- 有子菜单的一级菜单 -->
          <div v-if="m.children && m.children.length > 0" class="menuGroup">
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
          <RouterLink v-else class="navItem" :to="m.path">
            <span class="navIcon">
              <component :is="iconOf(m.icon)" />
            </span>
            <span>{{ m.menuName }}</span>
          </RouterLink>
        </template>
      </nav>

      <div class="sidebarFoot sf-muted">
        <div>后端：Spring Boot / MyBatis</div>
        <div>前端：Vue 3 / TypeScript</div>
      </div>
    </aside>

    <main class="main">
      <header class="topbar sf-card">
        <div class="left">
          <div class="chip sf-chip">
            <span class="dot"></span>
            <span>系统运行中</span>
          </div>
        </div>
        <div class="right">
          <div class="user">
            <div class="name">{{ auth.me?.displayName || auth.me?.username || "—" }}</div>
            <el-button size="small" text @click="logout">退出</el-button>
          </div>
        </div>
      </header>

      <div class="content sf-page">
        <RouterView />
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
  Setting
} from "@element-plus/icons-vue";
import { computed, ref } from "vue";
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

function logout() {
  auth.logout();
  router.replace("/login");
}
</script>

<style scoped>
.layout {
  height: 100vh;
  display: grid;
  grid-template-columns: 280px 1fr;
  gap: 16px;
  padding: 16px;
}

.sidebar {
  padding: 14px;
  display: flex;
  flex-direction: column;
}

.brand {
  display: flex;
  gap: 12px;
  align-items: center;
  padding: 10px 8px 14px 8px;
  border-bottom: 1px solid rgba(255, 255, 255, 0.12);
}

.logo {
  width: 38px;
  height: 38px;
  border-radius: 12px;
  background: radial-gradient(circle at 20% 20%, rgba(61, 214, 198, 0.9), transparent 60%),
    radial-gradient(circle at 75% 10%, rgba(42, 166, 255, 0.8), transparent 55%),
    rgba(255, 255, 255, 0.06);
  border: 1px solid rgba(255, 255, 255, 0.14);
  position: relative;
}

.logoDot {
  position: absolute;
  inset: 9px;
  border-radius: 10px;
  border: 1px dashed rgba(255, 255, 255, 0.18);
}

.brandTitle {
  font-weight: 700;
  font-size: 14px;
  letter-spacing: 0.4px;
}

.brandSub {
  margin-top: 2px;
  font-size: 12px;
  color: var(--sf-text-2);
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
  transition: 120ms ease;
  background: rgba(0, 0, 0, 0.08);
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
  color: rgba(255, 255, 255, 0.75);
}

.navItem:hover {
  border-color: rgba(255, 255, 255, 0.16);
  color: var(--sf-text-0);
}

.router-link-active {
  background: linear-gradient(135deg, rgba(61, 214, 198, 0.16), rgba(42, 166, 255, 0.12));
  border-color: rgba(61, 214, 198, 0.25);
  color: var(--sf-text-0);
}

.navItemParent.active {
  background: linear-gradient(135deg, rgba(61, 214, 198, 0.16), rgba(42, 166, 255, 0.12));
  border-color: rgba(61, 214, 198, 0.25);
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
  color: rgba(255, 255, 255, 0.95);
  padding: 8px 12px;
  border-radius: 10px;
  border: 1px solid rgba(255, 255, 255, 0.1);
  transition: 120ms ease;
  background: rgba(255, 255, 255, 0.08);
  font-size: 13px;
  display: flex;
  align-items: center;
}

.subNavItem:hover {
  background: rgba(61, 214, 198, 0.2);
  color: #ffffff;
  border-color: rgba(61, 214, 198, 0.3);
}

.subNavItem.router-link-active {
  background: rgba(61, 214, 198, 0.3);
  color: #ffffff;
  border-color: rgba(61, 214, 198, 0.5);
  font-weight: 600;
  box-shadow: 0 0 12px rgba(61, 214, 198, 0.25);
}

.sidebarFoot {
  margin-top: auto;
  padding: 12px 8px 6px;
  border-top: 1px solid rgba(255, 255, 255, 0.12);
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
  padding: 12px 14px;
}

.chip .dot {
  width: 8px;
  height: 8px;
  border-radius: 999px;
  background: var(--sf-success);
  box-shadow: 0 0 0 6px rgba(51, 209, 122, 0.1);
}

.user {
  display: flex;
  align-items: center;
  gap: 10px;
}

.name {
  font-size: 13px;
  color: var(--sf-text-1);
}

.content {
  min-height: 0;
  flex: 1;
  overflow: auto;
}
</style>
