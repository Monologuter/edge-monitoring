package com.safetyfire.monitor.domain.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;

/**
 * 创建联动事件入参（FR-11）。
 */
public record LinkageCreateRequest(
        Long alarmId,
        @NotBlank @Size(max = 64) String linkageType,
        @NotBlank @Size(max = 255) String target,
        @Size(max = 512) String payload
) {
}

