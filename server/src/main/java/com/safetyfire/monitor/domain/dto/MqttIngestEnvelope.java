package com.safetyfire.monitor.domain.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;

/**
 * MQTT 接入统一包络（用于 MQTTX 发布）。
 *
 * 约定：
 * - data 为“业务 JSON 字符串”（不是对象），用于稳定签名与跨语言实现；
 * - signature 计算见 docs/api.md。
 */
public record MqttIngestEnvelope(
        @NotBlank String apiKey,
        @NotNull Long timestamp,
        @NotBlank String nonce,
        @NotBlank String signature,
        @NotBlank String data
) {
}

