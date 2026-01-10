package com.safetyfire.monitor.domain.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;

/**
 * 告警上报（摄像头 AI 盒子、人员点位系统等）。
 */
public record AlarmIngestRequest(
        @NotBlank(message = "告警类型不能为空") String alarmType,
        @NotBlank(message = "告警状态不能为空") String alarmStatus,
        @NotNull(message = "告警时间戳不能为空") Long warningTime,
        String deviceCode,
        String alarmFile
) {
}

