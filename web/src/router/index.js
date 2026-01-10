import { createRouter, createWebHistory } from "vue-router";
import { useAuthStore } from "@/store/auth";
import LoginView from "@/views/LoginView.vue";
import AppLayout from "@/views/layout/AppLayout.vue";
import DashboardView from "@/views/DashboardView.vue";
import AlarmCenterView from "@/views/AlarmCenterView.vue";
import DevicesView from "@/views/DevicesView.vue";
import DeviceMonitorView from "@/views/DeviceMonitorView.vue";
import CompanyView from "@/views/CompanyView.vue";
import PeopleView from "@/views/PeopleView.vue";
import CarsView from "@/views/CarsView.vue";
import StoresView from "@/views/StoresView.vue";
import CamerasView from "@/views/CamerasView.vue";
import ProvincialView from "@/views/ProvincialView.vue";
import AuditView from "@/views/AuditView.vue";
import ScreenView from "@/views/ScreenView.vue";
import AdminUsersView from "@/views/AdminUsersView.vue";
import AdminRolesView from "@/views/AdminRolesView.vue";
import HardwareIngestView from "@/views/HardwareIngestView.vue";
const router = createRouter({
    history: createWebHistory(),
    routes: [
        { path: "/login", name: "login", component: LoginView },
        {
            path: "/",
            component: AppLayout,
            children: [
                { path: "", redirect: "/dashboard" },
                { path: "dashboard", name: "dashboard", component: DashboardView },
                { path: "alarms", name: "alarms", component: AlarmCenterView },
                { path: "company", name: "company", component: CompanyView },
                { path: "people", name: "people", component: PeopleView },
                { path: "cars", name: "cars", component: CarsView },
                { path: "stores", name: "stores", component: StoresView },
                { path: "devices", name: "devices", component: DevicesView },
                { path: "devices/monitor", name: "deviceMonitor", component: DeviceMonitorView },
                { path: "cameras", name: "cameras", component: CamerasView },
                { path: "provincial", name: "provincial", component: ProvincialView },
                { path: "audit", name: "audit", component: AuditView },
                { path: "screen", name: "screen", component: ScreenView },
                { path: "hardware/ingest", name: "hardwareIngest", component: HardwareIngestView },
                { path: "admin/users", name: "adminUsers", component: AdminUsersView },
                { path: "admin/roles", name: "adminRoles", component: AdminRolesView }
            ]
        }
    ]
});
router.beforeEach(async (to) => {
    const auth = useAuthStore();
    if (to.path === "/login")
        return true;
    if (!auth.isLoggedIn) {
        auth.hydrate();
    }
    if (!auth.isLoggedIn)
        return "/login";
    if (!auth.meLoaded) {
        try {
            await auth.fetchMe();
        }
        catch {
            auth.logout();
            return "/login";
        }
    }
    return true;
});
export default router;
