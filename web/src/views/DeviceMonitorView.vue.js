import { onMounted, onUnmounted, ref } from "vue";
import { ElMessage } from "element-plus";
import { http } from "@/api/http";
import { connectStomp } from "@/realtime/stomp";
import * as echarts from "echarts";
const devices = ref([]);
const selectedDeviceCode = ref("");
const selectedDevice = ref(null);
const readings = ref(new Map());
const chartEl = ref(null);
let chart = null;
let stomp = null;
let sub = null;
function getDeviceTypeName(type) {
    const types = {
        1: "视频",
        2: "红外",
        3: "温度",
        4: "湿度",
        5: "液位"
    };
    return types[type] || "未知";
}
function getCurrentReading(deviceCode) {
    const arr = readings.value.get(deviceCode);
    if (!arr || arr.length === 0)
        return "-";
    return arr[arr.length - 1].realValue.toFixed(1);
}
async function loadDevices() {
    try {
        const data = await http.get("/api/v1/devices?page=1&pageSize=100");
        devices.value = data.list;
    }
    catch (e) {
        ElMessage.error(e?.message || "加载设备失败");
    }
}
function onDeviceChange() {
    if (selectedDeviceCode.value) {
        selectedDevice.value = devices.value.find(d => d.deviceCode === selectedDeviceCode.value) || null;
        loadTrendData();
    }
}
async function loadTrendData() {
    if (!selectedDeviceCode.value || !chartEl.value)
        return;
    try {
        const now = Date.now();
        const hourStart = now - (now % 3600000);
        const data = await http.get(`/api/v1/device-readings/by-device/${selectedDeviceCode.value}?hours=24`);
        if (!chart) {
            chart = echarts.init(chartEl.value);
        }
        const option = {
            title: { text: "24小时趋势", left: "center" },
            tooltip: { trigger: "axis" },
            xAxis: {
                type: "category",
                data: data.list.map((d) => new Date(d.hourStart).getHours() + ":00")
            },
            yAxis: { type: "value" },
            series: [{
                    data: data.list.map((d) => d.sampleValue),
                    type: "line",
                    smooth: true,
                    areaStyle: { opacity: 0.3 }
                }]
        };
        chart.setOption(option);
    }
    catch (e) {
        console.error("加载趋势数据失败", e);
    }
}
function setupRealtimeSubscription() {
    stomp = connectStomp(() => { });
    stomp.activate();
    sub = stomp.subscribeJson("/topic/device-readings", (reading) => {
        const arr = readings.value.get(reading.deviceCode) || [];
        arr.push(reading);
        // 只保留最近100条
        if (arr.length > 100)
            arr.shift();
        readings.value.set(reading.deviceCode, arr);
        // 如果正在查看该设备的详情，更新图表
        if (selectedDeviceCode.value === reading.deviceCode) {
            loadTrendData();
        }
    });
}
onMounted(() => {
    loadDevices();
    setupRealtimeSubscription();
});
onUnmounted(() => {
    sub?.unsubscribe();
    stomp?.deactivate();
    chart?.dispose();
});
debugger; /* PartiallyEnd: #3632/scriptSetup.vue */
const __VLS_ctx = {};
let __VLS_components;
let __VLS_directives;
/** @type {__VLS_StyleScopedClasses['device-card']} */ ;
/** @type {__VLS_StyleScopedClasses['device-card']} */ ;
/** @type {__VLS_StyleScopedClasses['device-status']} */ ;
/** @type {__VLS_StyleScopedClasses['reading-value']} */ ;
/** @type {__VLS_StyleScopedClasses['reading-value']} */ ;
/** @type {__VLS_StyleScopedClasses['detail-header']} */ ;
/** @type {__VLS_StyleScopedClasses['status-badge']} */ ;
/** @type {__VLS_StyleScopedClasses['online']} */ ;
/** @type {__VLS_StyleScopedClasses['info-item']} */ ;
// CSS variable injection 
// CSS variable injection end 
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
const __VLS_0 = {}.ElSelect;
/** @type {[typeof __VLS_components.ElSelect, typeof __VLS_components.elSelect, typeof __VLS_components.ElSelect, typeof __VLS_components.elSelect, ]} */ ;
// @ts-ignore
const __VLS_1 = __VLS_asFunctionalComponent(__VLS_0, new __VLS_0({
    ...{ 'onChange': {} },
    modelValue: (__VLS_ctx.selectedDeviceCode),
    placeholder: "选择设备",
    clearable: true,
    filterable: true,
    ...{ style: {} },
}));
const __VLS_2 = __VLS_1({
    ...{ 'onChange': {} },
    modelValue: (__VLS_ctx.selectedDeviceCode),
    placeholder: "选择设备",
    clearable: true,
    filterable: true,
    ...{ style: {} },
}, ...__VLS_functionalComponentArgsRest(__VLS_1));
let __VLS_4;
let __VLS_5;
let __VLS_6;
const __VLS_7 = {
    onChange: (__VLS_ctx.onDeviceChange)
};
__VLS_3.slots.default;
for (const [d] of __VLS_getVForSourceType((__VLS_ctx.devices))) {
    const __VLS_8 = {}.ElOption;
    /** @type {[typeof __VLS_components.ElOption, typeof __VLS_components.elOption, ]} */ ;
    // @ts-ignore
    const __VLS_9 = __VLS_asFunctionalComponent(__VLS_8, new __VLS_8({
        key: (d.deviceCode),
        label: (`${d.deviceName} (${d.deviceCode})`),
        value: (d.deviceCode),
    }));
    const __VLS_10 = __VLS_9({
        key: (d.deviceCode),
        label: (`${d.deviceName} (${d.deviceCode})`),
        value: (d.deviceCode),
    }, ...__VLS_functionalComponentArgsRest(__VLS_9));
}
var __VLS_3;
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
    onClick: (__VLS_ctx.loadDevices)
};
__VLS_15.slots.default;
var __VLS_15;
if (!__VLS_ctx.selectedDeviceCode) {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
        ...{ class: "device-grid" },
    });
    for (const [d] of __VLS_getVForSourceType((__VLS_ctx.devices))) {
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
            key: (d.deviceCode),
            ...{ class: "device-card" },
            ...{ class: ({ offline: d.onlineStatus !== 1 }) },
        });
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
            ...{ class: "device-card-header" },
        });
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({
            ...{ class: "device-name" },
        });
        (d.deviceName);
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({
            ...{ class: "device-status" },
            ...{ class: ({ online: d.onlineStatus === 1 }) },
        });
        (d.onlineStatus === 1 ? '在线' : '离线');
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
            ...{ class: "device-card-body" },
        });
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
            ...{ class: "device-code" },
        });
        (d.deviceCode);
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
            ...{ class: "device-location" },
        });
        (d.locationName || '-');
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
            ...{ class: "device-type" },
        });
        (__VLS_ctx.getDeviceTypeName(d.deviceType));
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
            ...{ class: "device-card-footer" },
        });
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
            ...{ class: "reading-value" },
        });
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({
            ...{ class: "value" },
        });
        (__VLS_ctx.getCurrentReading(d.deviceCode));
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({
            ...{ class: "unit" },
        });
        (d.unit || '');
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
            ...{ class: "threshold-info" },
        });
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({
            ...{ class: "threshold" },
        });
        (d.lowerLimit ?? '-');
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({
            ...{ class: "threshold" },
        });
        (d.upperLimit ?? '-');
    }
}
else {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
        ...{ class: "device-detail" },
    });
    const __VLS_20 = {}.ElButton;
    /** @type {[typeof __VLS_components.ElButton, typeof __VLS_components.elButton, typeof __VLS_components.ElButton, typeof __VLS_components.elButton, ]} */ ;
    // @ts-ignore
    const __VLS_21 = __VLS_asFunctionalComponent(__VLS_20, new __VLS_20({
        ...{ 'onClick': {} },
        ...{ style: {} },
    }));
    const __VLS_22 = __VLS_21({
        ...{ 'onClick': {} },
        ...{ style: {} },
    }, ...__VLS_functionalComponentArgsRest(__VLS_21));
    let __VLS_24;
    let __VLS_25;
    let __VLS_26;
    const __VLS_27 = {
        onClick: (...[$event]) => {
            if (!!(!__VLS_ctx.selectedDeviceCode))
                return;
            __VLS_ctx.selectedDeviceCode = '';
        }
    };
    __VLS_23.slots.default;
    var __VLS_23;
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
        ...{ class: "detail-card" },
    });
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
        ...{ class: "detail-header" },
    });
    __VLS_asFunctionalElement(__VLS_intrinsicElements.h3, __VLS_intrinsicElements.h3)({});
    (__VLS_ctx.selectedDevice?.deviceName);
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({
        ...{ class: "status-badge" },
        ...{ class: ({ online: __VLS_ctx.selectedDevice?.onlineStatus === 1 }) },
    });
    (__VLS_ctx.selectedDevice?.onlineStatus === 1 ? '在线' : '离线');
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
        ...{ class: "detail-info" },
    });
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
        ...{ class: "info-item" },
    });
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({
        ...{ class: "label" },
    });
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
    (__VLS_ctx.selectedDevice?.deviceCode);
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
        ...{ class: "info-item" },
    });
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({
        ...{ class: "label" },
    });
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
    (__VLS_ctx.selectedDevice?.locationName || '-');
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
        ...{ class: "info-item" },
    });
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({
        ...{ class: "label" },
    });
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
    (__VLS_ctx.selectedDevice?.deviceType !== undefined ? __VLS_ctx.getDeviceTypeName(__VLS_ctx.selectedDevice.deviceType) : '-');
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
        ...{ class: "info-item" },
    });
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({
        ...{ class: "label" },
    });
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
    (__VLS_ctx.selectedDevice?.lowerLimit ?? '-');
    (__VLS_ctx.selectedDevice?.upperLimit ?? '-');
    (__VLS_ctx.selectedDevice?.unit || '');
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
        ...{ class: "detail-chart" },
    });
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
        ref: "chartEl",
        ...{ style: {} },
    });
    /** @type {typeof __VLS_ctx.chartEl} */ ;
}
/** @type {__VLS_StyleScopedClasses['sf-title']} */ ;
/** @type {__VLS_StyleScopedClasses['device-grid']} */ ;
/** @type {__VLS_StyleScopedClasses['device-card']} */ ;
/** @type {__VLS_StyleScopedClasses['device-card-header']} */ ;
/** @type {__VLS_StyleScopedClasses['device-name']} */ ;
/** @type {__VLS_StyleScopedClasses['device-status']} */ ;
/** @type {__VLS_StyleScopedClasses['device-card-body']} */ ;
/** @type {__VLS_StyleScopedClasses['device-code']} */ ;
/** @type {__VLS_StyleScopedClasses['device-location']} */ ;
/** @type {__VLS_StyleScopedClasses['device-type']} */ ;
/** @type {__VLS_StyleScopedClasses['device-card-footer']} */ ;
/** @type {__VLS_StyleScopedClasses['reading-value']} */ ;
/** @type {__VLS_StyleScopedClasses['value']} */ ;
/** @type {__VLS_StyleScopedClasses['unit']} */ ;
/** @type {__VLS_StyleScopedClasses['threshold-info']} */ ;
/** @type {__VLS_StyleScopedClasses['threshold']} */ ;
/** @type {__VLS_StyleScopedClasses['threshold']} */ ;
/** @type {__VLS_StyleScopedClasses['device-detail']} */ ;
/** @type {__VLS_StyleScopedClasses['detail-card']} */ ;
/** @type {__VLS_StyleScopedClasses['detail-header']} */ ;
/** @type {__VLS_StyleScopedClasses['status-badge']} */ ;
/** @type {__VLS_StyleScopedClasses['detail-info']} */ ;
/** @type {__VLS_StyleScopedClasses['info-item']} */ ;
/** @type {__VLS_StyleScopedClasses['label']} */ ;
/** @type {__VLS_StyleScopedClasses['info-item']} */ ;
/** @type {__VLS_StyleScopedClasses['label']} */ ;
/** @type {__VLS_StyleScopedClasses['info-item']} */ ;
/** @type {__VLS_StyleScopedClasses['label']} */ ;
/** @type {__VLS_StyleScopedClasses['info-item']} */ ;
/** @type {__VLS_StyleScopedClasses['label']} */ ;
/** @type {__VLS_StyleScopedClasses['detail-chart']} */ ;
var __VLS_dollars;
const __VLS_self = (await import('vue')).defineComponent({
    setup() {
        return {
            devices: devices,
            selectedDeviceCode: selectedDeviceCode,
            selectedDevice: selectedDevice,
            chartEl: chartEl,
            getDeviceTypeName: getDeviceTypeName,
            getCurrentReading: getCurrentReading,
            loadDevices: loadDevices,
            onDeviceChange: onDeviceChange,
        };
    },
});
export default (await import('vue')).defineComponent({
    setup() {
        return {};
    },
});
; /* PartiallyEnd: #4569/main.vue */
