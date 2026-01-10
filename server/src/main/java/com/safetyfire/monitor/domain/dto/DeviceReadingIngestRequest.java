package com.safetyfire.monitor.domain.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;

/**
 * 设备实时数据上报（串口服务器/边缘设备推送）。
 */
public record DeviceReadingIngestRequest(
        @NotBlank(message = "设备编码不能为空") String deviceCode,
        @NotNull(message = "实时值不能为空") Double realValue,
        @NotNull(message = "上报时间戳不能为空") Long systime
) {
}

