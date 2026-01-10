import { useAuthStore } from "@/store/auth";
import router from "@/router";
import { Bell, Box, Connection, Cpu, DataLine, Document, Monitor, OfficeBuilding, User, Van, VideoCamera, Setting } from "@element-plus/icons-vue";
import { computed, ref } from "vue";
import { useRoute } from "vue-router";
const auth = useAuthStore();
const route = useRoute();
const menus = computed(() => auth.me?.menus || []);
const expandedMenus = ref([]);
const iconMap = {
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
function iconOf(name) {
    if (!name)
        return Monitor;
    return iconMap[name] || Monitor;
}
function toggleMenu(menuId) {
    const index = expandedMenus.value.indexOf(menuId);
    if (index === -1) {
        expandedMenus.value.push(menuId);
    }
    else {
        expandedMenus.value.splice(index, 1);
    }
}
function isMenuActive(menu) {
    if (!menu.children)
        return false;
    const currentPath = route.path;
    return menu.children.some((sub) => sub.path === currentPath);
}
function logout() {
    auth.logout();
    router.replace("/login");
}
debugger; /* PartiallyEnd: #3632/scriptSetup.vue */
const __VLS_ctx = {};
let __VLS_components;
let __VLS_directives;
/** @type {__VLS_StyleScopedClasses['arrow']} */ ;
/** @type {__VLS_StyleScopedClasses['navItem']} */ ;
/** @type {__VLS_StyleScopedClasses['navItemParent']} */ ;
/** @type {__VLS_StyleScopedClasses['subNavItem']} */ ;
/** @type {__VLS_StyleScopedClasses['subNavItem']} */ ;
/** @type {__VLS_StyleScopedClasses['router-link-active']} */ ;
// CSS variable injection 
// CSS variable injection end 
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "layout" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.aside, __VLS_intrinsicElements.aside)({
    ...{ class: "sidebar sf-card" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "brand" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "logo" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "logoDot" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "brandText" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "brandTitle" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "brandSub" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.nav, __VLS_intrinsicElements.nav)({
    ...{ class: "nav" },
});
for (const [m] of __VLS_getVForSourceType((__VLS_ctx.menus))) {
    (m.id);
    if (m.children && m.children.length > 0) {
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
            ...{ class: "menuGroup" },
        });
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
            ...{ onClick: (...[$event]) => {
                    if (!(m.children && m.children.length > 0))
                        return;
                    __VLS_ctx.toggleMenu(m.id);
                } },
            ...{ class: "navItem navItemParent" },
            ...{ class: ({ active: __VLS_ctx.isMenuActive(m) }) },
        });
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({
            ...{ class: "navIcon" },
        });
        const __VLS_0 = ((__VLS_ctx.iconOf(m.icon)));
        // @ts-ignore
        const __VLS_1 = __VLS_asFunctionalComponent(__VLS_0, new __VLS_0({}));
        const __VLS_2 = __VLS_1({}, ...__VLS_functionalComponentArgsRest(__VLS_1));
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({
            ...{ class: "navLabel" },
        });
        (m.menuName);
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({
            ...{ class: "arrow" },
            ...{ class: ({ expanded: __VLS_ctx.expandedMenus.includes(m.id) }) },
        });
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
            ...{ class: "subMenu" },
        });
        __VLS_asFunctionalDirective(__VLS_directives.vShow)(null, { ...__VLS_directiveBindingRestFields, value: (__VLS_ctx.expandedMenus.includes(m.id)) }, null, null);
        for (const [sub] of __VLS_getVForSourceType((m.children))) {
            const __VLS_4 = {}.RouterLink;
            /** @type {[typeof __VLS_components.RouterLink, typeof __VLS_components.RouterLink, ]} */ ;
            // @ts-ignore
            const __VLS_5 = __VLS_asFunctionalComponent(__VLS_4, new __VLS_4({
                key: (sub.id),
                ...{ class: "subNavItem" },
                to: (sub.path),
            }));
            const __VLS_6 = __VLS_5({
                key: (sub.id),
                ...{ class: "subNavItem" },
                to: (sub.path),
            }, ...__VLS_functionalComponentArgsRest(__VLS_5));
            __VLS_7.slots.default;
            __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
            (sub.menuName);
            var __VLS_7;
        }
    }
    else {
        const __VLS_8 = {}.RouterLink;
        /** @type {[typeof __VLS_components.RouterLink, typeof __VLS_components.RouterLink, ]} */ ;
        // @ts-ignore
        const __VLS_9 = __VLS_asFunctionalComponent(__VLS_8, new __VLS_8({
            ...{ class: "navItem" },
            to: (m.path),
        }));
        const __VLS_10 = __VLS_9({
            ...{ class: "navItem" },
            to: (m.path),
        }, ...__VLS_functionalComponentArgsRest(__VLS_9));
        __VLS_11.slots.default;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({
            ...{ class: "navIcon" },
        });
        const __VLS_12 = ((__VLS_ctx.iconOf(m.icon)));
        // @ts-ignore
        const __VLS_13 = __VLS_asFunctionalComponent(__VLS_12, new __VLS_12({}));
        const __VLS_14 = __VLS_13({}, ...__VLS_functionalComponentArgsRest(__VLS_13));
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
        (m.menuName);
        var __VLS_11;
    }
}
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sidebarFoot sf-muted" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
__VLS_asFunctionalElement(__VLS_intrinsicElements.main, __VLS_intrinsicElements.main)({
    ...{ class: "main" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.header, __VLS_intrinsicElements.header)({
    ...{ class: "topbar sf-card" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "left" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "chip sf-chip" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({
    ...{ class: "dot" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "right" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "user" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "name" },
});
(__VLS_ctx.auth.me?.displayName || __VLS_ctx.auth.me?.username || "—");
const __VLS_16 = {}.ElButton;
/** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
// @ts-ignore
const __VLS_17 = __VLS_asFunctionalComponent(__VLS_16, new __VLS_16({
    ...{ 'onClick': {} },
    size: "small",
    text: true,
}));
const __VLS_18 = __VLS_17({
    ...{ 'onClick': {} },
    size: "small",
    text: true,
}, ...__VLS_functionalComponentArgsRest(__VLS_17));
let __VLS_20;
let __VLS_21;
let __VLS_22;
const __VLS_23 = {
    onClick: (__VLS_ctx.logout)
};
__VLS_19.slots.default;
var __VLS_19;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "content sf-page" },
});
const __VLS_24 = {}.RouterView;
/** @type {[typeof __VLS_components.RouterView, ]} */ ;
// @ts-ignore
const __VLS_25 = __VLS_asFunctionalComponent(__VLS_24, new __VLS_24({}));
const __VLS_26 = __VLS_25({}, ...__VLS_functionalComponentArgsRest(__VLS_25));
/** @type {__VLS_StyleScopedClasses['layout']} */ ;
/** @type {__VLS_StyleScopedClasses['sidebar']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-card']} */ ;
/** @type {__VLS_StyleScopedClasses['brand']} */ ;
/** @type {__VLS_StyleScopedClasses['logo']} */ ;
/** @type {__VLS_StyleScopedClasses['logoDot']} */ ;
/** @type {__VLS_StyleScopedClasses['brandText']} */ ;
/** @type {__VLS_StyleScopedClasses['brandTitle']} */ ;
/** @type {__VLS_StyleScopedClasses['brandSub']} */ ;
/** @type {__VLS_StyleScopedClasses['nav']} */ ;
/** @type {__VLS_StyleScopedClasses['menuGroup']} */ ;
/** @type {__VLS_StyleScopedClasses['navItem']} */ ;
/** @type {__VLS_StyleScopedClasses['navItemParent']} */ ;
/** @type {__VLS_StyleScopedClasses['navIcon']} */ ;
/** @type {__VLS_StyleScopedClasses['navLabel']} */ ;
/** @type {__VLS_StyleScopedClasses['arrow']} */ ;
/** @type {__VLS_StyleScopedClasses['subMenu']} */ ;
/** @type {__VLS_StyleScopedClasses['subNavItem']} */ ;
/** @type {__VLS_StyleScopedClasses['navItem']} */ ;
/** @type {__VLS_StyleScopedClasses['navIcon']} */ ;
/** @type {__VLS_StyleScopedClasses['sidebarFoot']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-muted']} */ ;
/** @type {__VLS_StyleScopedClasses['main']} */ ;
/** @type {__VLS_StyleScopedClasses['topbar']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-card']} */ ;
/** @type {__VLS_StyleScopedClasses['left']} */ ;
/** @type {__VLS_StyleScopedClasses['chip']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-chip']} */ ;
/** @type {__VLS_StyleScopedClasses['dot']} */ ;
/** @type {__VLS_StyleScopedClasses['right']} */ ;
/** @type {__VLS_StyleScopedClasses['user']} */ ;
/** @type {__VLS_StyleScopedClasses['name']} */ ;
/** @type {__VLS_StyleScopedClasses['content']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-page']} */ ;
var __VLS_dollars;
const __VLS_self = (await import('vue')).defineComponent({
    setup() {
        return {
            auth: auth,
            menus: menus,
            expandedMenus: expandedMenus,
            iconOf: iconOf,
            toggleMenu: toggleMenu,
            isMenuActive: isMenuActive,
            logout: logout,
        };
    },
});
export default (await import('vue')).defineComponent({
    setup() {
        return {};
    },
});
; /* PartiallyEnd: #4569/main.vue */
