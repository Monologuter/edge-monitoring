package com.safetyfire.monitor.domain.dto;

import java.util.List;

/**
 * AI盒子心跳请求（字段按文档示例保留，后续可按实际补齐）。
 */
public record AiBoxHeartbeatRequest(
        String deviceSerial,
        Integer deviceSize,
        List<DeviceStatus> devices
) {
    public record DeviceStatus(
            String id,
            String name,
            String reason,
            Integer status
    ) {
    }
}

