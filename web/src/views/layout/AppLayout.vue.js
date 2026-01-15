import { useAuthStore } from "@/store/auth";
import router from "@/router";
import { Bell, Box, Connection, Cpu, DataLine, Document, Monitor, OfficeBuilding, User, Van, VideoCamera, Setting, Search, ArrowDown } from "@element-plus/icons-vue";
import { computed, ref, watch } from "vue";
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
const currentTitle = computed(() => {
    const currentPath = route.path;
    for (const menu of menus.value) {
        if (menu.path === currentPath)
            return menu.menuName;
        if (menu.children?.length) {
            const found = menu.children.find((sub) => sub.path === currentPath);
            if (found)
                return found.menuName;
        }
    }
    return "数据看板";
});
const userInitial = computed(() => {
    const name = auth.me?.displayName || auth.me?.username || "管";
    return name.slice(0, 1);
});
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
function syncExpandedMenus() {
    const currentPath = route.path;
    const parent = menus.value.find((menu) => menu.children?.some((sub) => sub.path === currentPath));
    if (!parent)
        return;
    if (!expandedMenus.value.includes(parent.id)) {
        expandedMenus.value.push(parent.id);
    }
}
watch(() => [menus.value, route.path], () => syncExpandedMenus(), { immediate: true });
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
/** @type {__VLS_StyleScopedClasses['layout']} */ ;
/** @type {__VLS_StyleScopedClasses['sidebar']} */ ;
/** @type {__VLS_StyleScopedClasses['nav']} */ ;
/** @type {__VLS_StyleScopedClasses['menuGroup']} */ ;
/** @type {__VLS_StyleScopedClasses['navItem']} */ ;
/** @type {__VLS_StyleScopedClasses['topbar']} */ ;
/** @type {__VLS_StyleScopedClasses['topActions']} */ ;
/** @type {__VLS_StyleScopedClasses['searchInput']} */ ;
// CSS variable injection 
// CSS variable injection end 
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "layout" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.aside, __VLS_intrinsicElements.aside)({
    ...{ class: "sidebar sf-card sf-glow" },
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
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "navTitle" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.nav, __VLS_intrinsicElements.nav)({
    ...{ class: "nav sf-stagger" },
    ...{ style: {} },
});
for (const [m] of __VLS_getVForSourceType((__VLS_ctx.menus))) {
    (m.id);
    if (m.children && m.children.length > 0) {
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
            ...{ class: "menuGroup" },
            ...{ style: {} },
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
            ...{ style: {} },
        }));
        const __VLS_10 = __VLS_9({
            ...{ class: "navItem" },
            to: (m.path),
            ...{ style: {} },
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
    ...{ class: "sidebarActions" },
});
const __VLS_16 = {}.ElButton;
/** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
// @ts-ignore
const __VLS_17 = __VLS_asFunctionalComponent(__VLS_16, new __VLS_16({
    type: "primary",
    plain: true,
    size: "small",
    ...{ class: "actionBtn" },
}));
const __VLS_18 = __VLS_17({
    type: "primary",
    plain: true,
    size: "small",
    ...{ class: "actionBtn" },
}, ...__VLS_functionalComponentArgsRest(__VLS_17));
__VLS_19.slots.default;
var __VLS_19;
const __VLS_20 = {}.ElButton;
/** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
// @ts-ignore
const __VLS_21 = __VLS_asFunctionalComponent(__VLS_20, new __VLS_20({
    size: "small",
    ...{ class: "actionBtn" },
}));
const __VLS_22 = __VLS_21({
    size: "small",
    ...{ class: "actionBtn" },
}, ...__VLS_functionalComponentArgsRest(__VLS_21));
__VLS_23.slots.default;
var __VLS_23;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sidebarFoot sf-muted" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
__VLS_asFunctionalElement(__VLS_intrinsicElements.main, __VLS_intrinsicElements.main)({
    ...{ class: "main" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.header, __VLS_intrinsicElements.header)({
    ...{ class: "topbar sf-card sf-glow" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "pageInfo" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "pageTitle" },
});
(__VLS_ctx.currentTitle);
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "pageSub" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "topActions" },
});
const __VLS_24 = {}.ElInput;
/** @type {[typeof __VLS_components.ElInput, typeof __VLS_components.elInput, typeof __VLS_components.ElInput, typeof __VLS_components.elInput, ]} */ ;
// @ts-ignore
const __VLS_25 = __VLS_asFunctionalComponent(__VLS_24, new __VLS_24({
    ...{ class: "searchInput" },
    placeholder: "搜索今日报警事件",
}));
const __VLS_26 = __VLS_25({
    ...{ class: "searchInput" },
    placeholder: "搜索今日报警事件",
}, ...__VLS_functionalComponentArgsRest(__VLS_25));
__VLS_27.slots.default;
{
    const { prefix: __VLS_thisSlot } = __VLS_27.slots;
    const __VLS_28 = {}.ElIcon;
    /** @type {[typeof __VLS_components.ElIcon, typeof __VLS_components.elIcon, typeof __VLS_components.ElIcon, typeof __VLS_components.elIcon, ]} */ ;
    // @ts-ignore
    const __VLS_29 = __VLS_asFunctionalComponent(__VLS_28, new __VLS_28({}));
    const __VLS_30 = __VLS_29({}, ...__VLS_functionalComponentArgsRest(__VLS_29));
    __VLS_31.slots.default;
    const __VLS_32 = {}.Search;
    /** @type {[typeof __VLS_components.Search, ]} */ ;
    // @ts-ignore
    const __VLS_33 = __VLS_asFunctionalComponent(__VLS_32, new __VLS_32({}));
    const __VLS_34 = __VLS_33({}, ...__VLS_functionalComponentArgsRest(__VLS_33));
    var __VLS_31;
}
var __VLS_27;
const __VLS_36 = {}.ElButton;
/** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
// @ts-ignore
const __VLS_37 = __VLS_asFunctionalComponent(__VLS_36, new __VLS_36({
    ...{ class: "iconBtn" },
    text: true,
    circle: true,
}));
const __VLS_38 = __VLS_37({
    ...{ class: "iconBtn" },
    text: true,
    circle: true,
}, ...__VLS_functionalComponentArgsRest(__VLS_37));
__VLS_39.slots.default;
const __VLS_40 = {}.ElIcon;
/** @type {[typeof __VLS_components.ElIcon, typeof __VLS_components.elIcon, typeof __VLS_components.ElIcon, typeof __VLS_components.elIcon, ]} */ ;
// @ts-ignore
const __VLS_41 = __VLS_asFunctionalComponent(__VLS_40, new __VLS_40({}));
const __VLS_42 = __VLS_41({}, ...__VLS_functionalComponentArgsRest(__VLS_41));
__VLS_43.slots.default;
const __VLS_44 = {}.Bell;
/** @type {[typeof __VLS_components.Bell, ]} */ ;
// @ts-ignore
const __VLS_45 = __VLS_asFunctionalComponent(__VLS_44, new __VLS_44({}));
const __VLS_46 = __VLS_45({}, ...__VLS_functionalComponentArgsRest(__VLS_45));
var __VLS_43;
var __VLS_39;
const __VLS_48 = {}.ElDropdown;
/** @type {[typeof __VLS_components.ElDropdown, typeof __VLS_components.elDropdown, typeof __VLS_components.ElDropdown, typeof __VLS_components.elDropdown, ]} */ ;
// @ts-ignore
const __VLS_49 = __VLS_asFunctionalComponent(__VLS_48, new __VLS_48({}));
const __VLS_50 = __VLS_49({}, ...__VLS_functionalComponentArgsRest(__VLS_49));
__VLS_51.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "user" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "avatar" },
});
(__VLS_ctx.userInitial);
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "name" },
});
(__VLS_ctx.auth.me?.displayName || __VLS_ctx.auth.me?.username || "管理员");
const __VLS_52 = {}.ElIcon;
/** @type {[typeof __VLS_components.ElIcon, typeof __VLS_components.elIcon, typeof __VLS_components.ElIcon, typeof __VLS_components.elIcon, ]} */ ;
// @ts-ignore
const __VLS_53 = __VLS_asFunctionalComponent(__VLS_52, new __VLS_52({
    ...{ class: "arrowIcon" },
}));
const __VLS_54 = __VLS_53({
    ...{ class: "arrowIcon" },
}, ...__VLS_functionalComponentArgsRest(__VLS_53));
__VLS_55.slots.default;
const __VLS_56 = {}.ArrowDown;
/** @type {[typeof __VLS_components.ArrowDown, ]} */ ;
// @ts-ignore
const __VLS_57 = __VLS_asFunctionalComponent(__VLS_56, new __VLS_56({}));
const __VLS_58 = __VLS_57({}, ...__VLS_functionalComponentArgsRest(__VLS_57));
var __VLS_55;
{
    const { dropdown: __VLS_thisSlot } = __VLS_51.slots;
    const __VLS_60 = {}.ElDropdownMenu;
    /** @type {[typeof __VLS_components.ElDropdownMenu, typeof __VLS_components.elDropdownMenu, typeof __VLS_components.ElDropdownMenu, typeof __VLS_components.elDropdownMenu, ]} */ ;
    // @ts-ignore
    const __VLS_61 = __VLS_asFunctionalComponent(__VLS_60, new __VLS_60({}));
    const __VLS_62 = __VLS_61({}, ...__VLS_functionalComponentArgsRest(__VLS_61));
    __VLS_63.slots.default;
    const __VLS_64 = {}.ElDropdownItem;
    /** @type {[typeof __VLS_components.ElDropdownItem, typeof __VLS_components.elDropdownItem, typeof __VLS_components.ElDropdownItem, typeof __VLS_components.elDropdownItem, ]} */ ;
    // @ts-ignore
    const __VLS_65 = __VLS_asFunctionalComponent(__VLS_64, new __VLS_64({}));
    const __VLS_66 = __VLS_65({}, ...__VLS_functionalComponentArgsRest(__VLS_65));
    __VLS_67.slots.default;
    var __VLS_67;
    const __VLS_68 = {}.ElDropdownItem;
    /** @type {[typeof __VLS_components.ElDropdownItem, typeof __VLS_components.elDropdownItem, typeof __VLS_components.ElDropdownItem, typeof __VLS_components.elDropdownItem, ]} */ ;
    // @ts-ignore
    const __VLS_69 = __VLS_asFunctionalComponent(__VLS_68, new __VLS_68({
        ...{ 'onClick': {} },
        divided: true,
    }));
    const __VLS_70 = __VLS_69({
        ...{ 'onClick': {} },
        divided: true,
    }, ...__VLS_functionalComponentArgsRest(__VLS_69));
    let __VLS_72;
    let __VLS_73;
    let __VLS_74;
    const __VLS_75 = {
        onClick: (__VLS_ctx.logout)
    };
    __VLS_71.slots.default;
    var __VLS_71;
    var __VLS_63;
}
var __VLS_51;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "content sf-page" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "pageFrame sf-panel sf-animate-in" },
    key: (__VLS_ctx.route.fullPath),
});
const __VLS_76 = {}.RouterView;
/** @type {[typeof __VLS_components.RouterView, ]} */ ;
// @ts-ignore
const __VLS_77 = __VLS_asFunctionalComponent(__VLS_76, new __VLS_76({}));
const __VLS_78 = __VLS_77({}, ...__VLS_functionalComponentArgsRest(__VLS_77));
/** @type {__VLS_StyleScopedClasses['layout']} */ ;
/** @type {__VLS_StyleScopedClasses['sidebar']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-card']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-glow']} */ ;
/** @type {__VLS_StyleScopedClasses['brand']} */ ;
/** @type {__VLS_StyleScopedClasses['logo']} */ ;
/** @type {__VLS_StyleScopedClasses['logoDot']} */ ;
/** @type {__VLS_StyleScopedClasses['brandText']} */ ;
/** @type {__VLS_StyleScopedClasses['brandTitle']} */ ;
/** @type {__VLS_StyleScopedClasses['brandSub']} */ ;
/** @type {__VLS_StyleScopedClasses['navTitle']} */ ;
/** @type {__VLS_StyleScopedClasses['nav']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-stagger']} */ ;
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
/** @type {__VLS_StyleScopedClasses['sidebarActions']} */ ;
/** @type {__VLS_StyleScopedClasses['actionBtn']} */ ;
/** @type {__VLS_StyleScopedClasses['actionBtn']} */ ;
/** @type {__VLS_StyleScopedClasses['sidebarFoot']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-muted']} */ ;
/** @type {__VLS_StyleScopedClasses['main']} */ ;
/** @type {__VLS_StyleScopedClasses['topbar']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-card']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-glow']} */ ;
/** @type {__VLS_StyleScopedClasses['pageInfo']} */ ;
/** @type {__VLS_StyleScopedClasses['pageTitle']} */ ;
/** @type {__VLS_StyleScopedClasses['pageSub']} */ ;
/** @type {__VLS_StyleScopedClasses['topActions']} */ ;
/** @type {__VLS_StyleScopedClasses['searchInput']} */ ;
/** @type {__VLS_StyleScopedClasses['iconBtn']} */ ;
/** @type {__VLS_StyleScopedClasses['user']} */ ;
/** @type {__VLS_StyleScopedClasses['avatar']} */ ;
/** @type {__VLS_StyleScopedClasses['name']} */ ;
/** @type {__VLS_StyleScopedClasses['arrowIcon']} */ ;
/** @type {__VLS_StyleScopedClasses['content']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-page']} */ ;
/** @type {__VLS_StyleScopedClasses['pageFrame']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-panel']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-animate-in']} */ ;
var __VLS_dollars;
const __VLS_self = (await import('vue')).defineComponent({
    setup() {
        return {
            Bell: Bell,
            Search: Search,
            ArrowDown: ArrowDown,
            auth: auth,
            route: route,
            menus: menus,
            expandedMenus: expandedMenus,
            currentTitle: currentTitle,
            userInitial: userInitial,
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
