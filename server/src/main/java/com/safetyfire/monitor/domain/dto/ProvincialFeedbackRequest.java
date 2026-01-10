package com.safetyfire.monitor.domain.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;

/**
 * 省厅预警反馈入参（FR-07）。
 */
public record ProvincialFeedbackRequest(
        @NotBlank @Size(max = 128) String externalId,
        @NotBlank @Size(max = 512) String feedback
) {
}

