package com.safetyfire.monitor.domain.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;

/**
 * 服务器心跳上报（FR-08/需求数据对象）。
 */
public record ServerHeartbeatIngestRequest(
        @NotBlank @Size(max = 64) String companyCode,
        @NotBlank @Size(max = 128) String computerName,
        @NotBlank @Size(max = 64) String ip,
        @Size(max = 64) String originalId,
        @NotNull Long heartbeatTime
) {
}

