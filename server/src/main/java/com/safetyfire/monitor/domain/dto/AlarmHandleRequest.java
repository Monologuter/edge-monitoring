package com.safetyfire.monitor.domain.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;

/**
 * 告警处理/消警入参。
 */
public record AlarmHandleRequest(
        @NotNull(message = "告警ID不能为空") Long alarmId,
        @NotBlank(message = "处理动作不能为空") String action,
        String remark
) {
}
