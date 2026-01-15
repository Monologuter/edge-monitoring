import { onMounted, ref } from "vue";
import * as echarts from "echarts";
import { http } from "@/api/http";
import { Cpu, Monitor, TrendCharts, Warning } from "@element-plus/icons-vue";
const data = ref(null);
const chartEl = ref(null);
const deviceChartEl = ref(null);
function buildHours() {
    const now = new Date();
    const hours = [];
    for (let i = 23; i >= 0; i--) {
        const d = new Date(now.getTime() - i * 3600 * 1000);
        hours.push(String(d.getHours()).padStart(2, "0") + ":00");
    }
    return hours;
}
async function load() {
    data.value = await http.get("/api/v1/dashboard/overview");
}
function renderAlarmTrend() {
    if (!chartEl.value || !data.value)
        return;
    const chart = echarts.init(chartEl.value);
    chart.setOption({
        backgroundColor: "transparent",
        grid: { left: 40, right: 18, top: 22, bottom: 34 },
        tooltip: { trigger: "axis" },
        xAxis: {
            type: "category",
            data: buildHours(),
            axisLabel: { color: "rgba(60, 73, 99, 0.6)" },
            axisLine: { lineStyle: { color: "rgba(70, 88, 124, 0.12)" } }
        },
        yAxis: {
            type: "value",
            axisLabel: { color: "rgba(60, 73, 99, 0.6)" },
            splitLine: { lineStyle: { color: "rgba(70, 88, 124, 0.08)" } }
        },
        series: [
            {
                name: "告警",
                type: "line",
                smooth: true,
                data: data.value.alarmTrend24h || [],
                lineStyle: { width: 3, color: "rgba(39,183,167,0.95)" },
                areaStyle: {
                    color: new echarts.graphic.LinearGradient(0, 0, 0, 1, [
                        { offset: 0, color: "rgba(39,183,167,0.24)" },
                        { offset: 1, color: "rgba(39,183,167,0.02)" }
                    ])
                },
                symbol: "circle",
                symbolSize: 8,
                itemStyle: { color: "rgba(47,107,255,0.95)" }
            }
        ]
    });
    window.addEventListener("resize", () => chart.resize());
}
function renderDeviceStatus() {
    if (!deviceChartEl.value || !data.value)
        return;
    const chart = echarts.init(deviceChartEl.value);
    const offline = Math.max(0, (data.value.totalDevices || 0) - (data.value.onlineDevices || 0));
    chart.setOption({
        backgroundColor: "transparent",
        tooltip: { trigger: "item" },
        series: [
            {
                type: "pie",
                radius: ["55%", "75%"],
                avoidLabelOverlap: false,
                label: { show: false },
                labelLine: { show: false },
                data: [
                    { value: data.value.onlineDevices || 0, name: "在线", itemStyle: { color: "rgba(47,107,255,0.9)" } },
                    { value: offline, name: "离线", itemStyle: { color: "rgba(255,107,107,0.7)" } }
                ]
            }
        ]
    });
    window.addEventListener("resize", () => chart.resize());
}
onMounted(async () => {
    await load();
    renderAlarmTrend();
    renderDeviceStatus();
});
debugger; /* PartiallyEnd: #3632/scriptSetup.vue */
const __VLS_ctx = {};
let __VLS_components;
let __VLS_directives;
/** @type {__VLS_StyleScopedClasses['kpiAlarm']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-kpi-icon']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-kpi-value']} */ ;
/** @type {__VLS_StyleScopedClasses['kpiOnline']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-kpi-icon']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-kpi-value']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-kpi-value']} */ ;
/** @type {__VLS_StyleScopedClasses['grid']} */ ;
/** @type {__VLS_StyleScopedClasses['stats']} */ ;
/** @type {__VLS_StyleScopedClasses['flow']} */ ;
/** @type {__VLS_StyleScopedClasses['stats']} */ ;
/** @type {__VLS_StyleScopedClasses['flow']} */ ;
// CSS variable injection 
// CSS variable injection end 
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-page dashboard" },
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
(__VLS_ctx.data?.companyName || "—");
(__VLS_ctx.data?.businessLicense || "—");
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-page-actions" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({
    ...{ class: "sf-badge" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-kpi sf-stagger" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-card sf-kpi-item kpiAlarm" },
    ...{ style: {} },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-kpi-head" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-kpi-title" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-kpi-sub" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-kpi-icon kpiAlarm" },
});
const __VLS_0 = {}.ElIcon;
/** @type {[typeof __VLS_components.ElIcon, typeof __VLS_components.elIcon, typeof __VLS_components.ElIcon, typeof __VLS_components.elIcon, ]} */ ;
// @ts-ignore
const __VLS_1 = __VLS_asFunctionalComponent(__VLS_0, new __VLS_0({}));
const __VLS_2 = __VLS_1({}, ...__VLS_functionalComponentArgsRest(__VLS_1));
__VLS_3.slots.default;
const __VLS_4 = {}.Warning;
/** @type {[typeof __VLS_components.Warning, ]} */ ;
// @ts-ignore
const __VLS_5 = __VLS_asFunctionalComponent(__VLS_4, new __VLS_4({}));
const __VLS_6 = __VLS_5({}, ...__VLS_functionalComponentArgsRest(__VLS_5));
var __VLS_3;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-kpi-value" },
});
(__VLS_ctx.data?.todayAlarms ?? 0);
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-card sf-kpi-item kpiOnline" },
    ...{ style: {} },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-kpi-head" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-kpi-title" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-kpi-sub" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-kpi-icon kpiOnline" },
});
const __VLS_8 = {}.ElIcon;
/** @type {[typeof __VLS_components.ElIcon, typeof __VLS_components.elIcon, typeof __VLS_components.ElIcon, typeof __VLS_components.elIcon, ]} */ ;
// @ts-ignore
const __VLS_9 = __VLS_asFunctionalComponent(__VLS_8, new __VLS_8({}));
const __VLS_10 = __VLS_9({}, ...__VLS_functionalComponentArgsRest(__VLS_9));
__VLS_11.slots.default;
const __VLS_12 = {}.Cpu;
/** @type {[typeof __VLS_components.Cpu, ]} */ ;
// @ts-ignore
const __VLS_13 = __VLS_asFunctionalComponent(__VLS_12, new __VLS_12({}));
const __VLS_14 = __VLS_13({}, ...__VLS_functionalComponentArgsRest(__VLS_13));
var __VLS_11;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-kpi-value" },
});
(__VLS_ctx.data?.onlineDevices ?? 0);
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-card sf-kpi-item kpiTotal" },
    ...{ style: {} },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-kpi-head" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-kpi-title" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-kpi-sub" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-kpi-icon kpiTotal" },
});
const __VLS_16 = {}.ElIcon;
/** @type {[typeof __VLS_components.ElIcon, typeof __VLS_components.elIcon, typeof __VLS_components.ElIcon, typeof __VLS_components.elIcon, ]} */ ;
// @ts-ignore
const __VLS_17 = __VLS_asFunctionalComponent(__VLS_16, new __VLS_16({}));
const __VLS_18 = __VLS_17({}, ...__VLS_functionalComponentArgsRest(__VLS_17));
__VLS_19.slots.default;
const __VLS_20 = {}.Monitor;
/** @type {[typeof __VLS_components.Monitor, ]} */ ;
// @ts-ignore
const __VLS_21 = __VLS_asFunctionalComponent(__VLS_20, new __VLS_20({}));
const __VLS_22 = __VLS_21({}, ...__VLS_functionalComponentArgsRest(__VLS_21));
var __VLS_19;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-kpi-value" },
});
(__VLS_ctx.data?.totalDevices ?? 0);
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-card sf-kpi-item kpiRate" },
    ...{ style: {} },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-kpi-head" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-kpi-title" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-kpi-sub" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-kpi-icon kpiRate" },
});
const __VLS_24 = {}.ElIcon;
/** @type {[typeof __VLS_components.ElIcon, typeof __VLS_components.elIcon, typeof __VLS_components.ElIcon, typeof __VLS_components.elIcon, ]} */ ;
// @ts-ignore
const __VLS_25 = __VLS_asFunctionalComponent(__VLS_24, new __VLS_24({}));
const __VLS_26 = __VLS_25({}, ...__VLS_functionalComponentArgsRest(__VLS_25));
__VLS_27.slots.default;
const __VLS_28 = {}.TrendCharts;
/** @type {[typeof __VLS_components.TrendCharts, ]} */ ;
// @ts-ignore
const __VLS_29 = __VLS_asFunctionalComponent(__VLS_28, new __VLS_28({}));
const __VLS_30 = __VLS_29({}, ...__VLS_functionalComponentArgsRest(__VLS_29));
var __VLS_27;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-kpi-value" },
});
((__VLS_ctx.data?.onlineRate ?? 0).toFixed(1));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "grid" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-card panel" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "panelTitle" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "stats" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "statItem" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "statLabel" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "statValue" },
});
(__VLS_ctx.data?.temperatureValue == null ? "-" : `${__VLS_ctx.data.temperatureValue}℃`);
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "statItem" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "statLabel" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "statValue" },
});
(__VLS_ctx.data?.humidityValue == null ? "-" : `${__VLS_ctx.data.humidityValue}%`);
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "statItem" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "statLabel" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "statValue" },
});
(__VLS_ctx.data?.levelValue == null ? "-" : `${__VLS_ctx.data.levelValue}m`);
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "statItem" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "statLabel" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "statValue" },
});
(__VLS_ctx.data?.onsitePeople ?? 0);
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "statItem" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "statLabel" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "statValue" },
});
(__VLS_ctx.data?.onsiteCars ?? 0);
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "statItem" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "statLabel" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "statValue" },
});
(__VLS_ctx.data?.storeroomCount ?? 0);
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "flow" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "flowItem" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "flowLabel" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "flowValue" },
});
(__VLS_ctx.data?.todayInPeople ?? 0);
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "flowItem" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "flowLabel" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "flowValue" },
});
(__VLS_ctx.data?.todayOutPeople ?? 0);
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "flowItem" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "flowLabel" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "flowValue" },
});
(__VLS_ctx.data?.todayInCars ?? 0);
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "flowItem" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "flowLabel" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "flowValue" },
});
(__VLS_ctx.data?.todayOutCars ?? 0);
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-card panel" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "panelTitle" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ref: "deviceChartEl",
    ...{ class: "chartSmall" },
});
/** @type {typeof __VLS_ctx.deviceChartEl} */ ;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "sf-card panel chartLarge" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ...{ class: "panelTitle" },
});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
    ref: "chartEl",
    ...{ class: "chartLargeInner" },
});
/** @type {typeof __VLS_ctx.chartEl} */ ;
/** @type {__VLS_StyleScopedClasses['sf-page']} */ ;
/** @type {__VLS_StyleScopedClasses['dashboard']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-page-head']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-page-title']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-page-sub']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-page-actions']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-badge']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-kpi']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-stagger']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-card']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-kpi-item']} */ ;
/** @type {__VLS_StyleScopedClasses['kpiAlarm']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-kpi-head']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-kpi-title']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-kpi-sub']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-kpi-icon']} */ ;
/** @type {__VLS_StyleScopedClasses['kpiAlarm']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-kpi-value']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-card']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-kpi-item']} */ ;
/** @type {__VLS_StyleScopedClasses['kpiOnline']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-kpi-head']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-kpi-title']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-kpi-sub']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-kpi-icon']} */ ;
/** @type {__VLS_StyleScopedClasses['kpiOnline']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-kpi-value']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-card']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-kpi-item']} */ ;
/** @type {__VLS_StyleScopedClasses['kpiTotal']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-kpi-head']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-kpi-title']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-kpi-sub']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-kpi-icon']} */ ;
/** @type {__VLS_StyleScopedClasses['kpiTotal']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-kpi-value']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-card']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-kpi-item']} */ ;
/** @type {__VLS_StyleScopedClasses['kpiRate']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-kpi-head']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-kpi-title']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-kpi-sub']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-kpi-icon']} */ ;
/** @type {__VLS_StyleScopedClasses['kpiRate']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-kpi-value']} */ ;
/** @type {__VLS_StyleScopedClasses['grid']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-card']} */ ;
/** @type {__VLS_StyleScopedClasses['panel']} */ ;
/** @type {__VLS_StyleScopedClasses['panelTitle']} */ ;
/** @type {__VLS_StyleScopedClasses['stats']} */ ;
/** @type {__VLS_StyleScopedClasses['statItem']} */ ;
/** @type {__VLS_StyleScopedClasses['statLabel']} */ ;
/** @type {__VLS_StyleScopedClasses['statValue']} */ ;
/** @type {__VLS_StyleScopedClasses['statItem']} */ ;
/** @type {__VLS_StyleScopedClasses['statLabel']} */ ;
/** @type {__VLS_StyleScopedClasses['statValue']} */ ;
/** @type {__VLS_StyleScopedClasses['statItem']} */ ;
/** @type {__VLS_StyleScopedClasses['statLabel']} */ ;
/** @type {__VLS_StyleScopedClasses['statValue']} */ ;
/** @type {__VLS_StyleScopedClasses['statItem']} */ ;
/** @type {__VLS_StyleScopedClasses['statLabel']} */ ;
/** @type {__VLS_StyleScopedClasses['statValue']} */ ;
/** @type {__VLS_StyleScopedClasses['statItem']} */ ;
/** @type {__VLS_StyleScopedClasses['statLabel']} */ ;
/** @type {__VLS_StyleScopedClasses['statValue']} */ ;
/** @type {__VLS_StyleScopedClasses['statItem']} */ ;
/** @type {__VLS_StyleScopedClasses['statLabel']} */ ;
/** @type {__VLS_StyleScopedClasses['statValue']} */ ;
/** @type {__VLS_StyleScopedClasses['flow']} */ ;
/** @type {__VLS_StyleScopedClasses['flowItem']} */ ;
/** @type {__VLS_StyleScopedClasses['flowLabel']} */ ;
/** @type {__VLS_StyleScopedClasses['flowValue']} */ ;
/** @type {__VLS_StyleScopedClasses['flowItem']} */ ;
/** @type {__VLS_StyleScopedClasses['flowLabel']} */ ;
/** @type {__VLS_StyleScopedClasses['flowValue']} */ ;
/** @type {__VLS_StyleScopedClasses['flowItem']} */ ;
/** @type {__VLS_StyleScopedClasses['flowLabel']} */ ;
/** @type {__VLS_StyleScopedClasses['flowValue']} */ ;
/** @type {__VLS_StyleScopedClasses['flowItem']} */ ;
/** @type {__VLS_StyleScopedClasses['flowLabel']} */ ;
/** @type {__VLS_StyleScopedClasses['flowValue']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-card']} */ ;
/** @type {__VLS_StyleScopedClasses['panel']} */ ;
/** @type {__VLS_StyleScopedClasses['panelTitle']} */ ;
/** @type {__VLS_StyleScopedClasses['chartSmall']} */ ;
/** @type {__VLS_StyleScopedClasses['sf-card']} */ ;
/** @type {__VLS_StyleScopedClasses['panel']} */ ;
/** @type {__VLS_StyleScopedClasses['chartLarge']} */ ;
/** @type {__VLS_StyleScopedClasses['panelTitle']} */ ;
/** @type {__VLS_StyleScopedClasses['chartLargeInner']} */ ;
var __VLS_dollars;
const __VLS_self = (await import('vue')).defineComponent({
    setup() {
        return {
            Cpu: Cpu,
            Monitor: Monitor,
            TrendCharts: TrendCharts,
            Warning: Warning,
            data: data,
            chartEl: chartEl,
            deviceChartEl: deviceChartEl,
        };
    },
});
export default (await import('vue')).defineComponent({
    setup() {
        return {};
    },
});
; /* PartiallyEnd: #4569/main.vue */
