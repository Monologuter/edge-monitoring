package com.safetyfire.monitor.domain.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;

/**
 * AI盒子告警上报（通道堵塞、超高超重、摄像头遮挡偏移等）。
 */
public record AiBoxAlarmIngestRequest(
        @NotBlank(message = "告警类型不能为空") String alarmType,
        @NotBlank(message = "告警状态不能为空") String alarmStatus,
        @NotNull(message = "告警时间戳不能为空") Long warningTime,
        String cameraCode,
        String areaCode,
        Double height,           // 超高报警：物体高度（米）
        Double weight,           // 超重报警：物体重量（吨）
        String description,      // 告警描述
        Long imageFileId         // 告警图片文件ID
) {
}
