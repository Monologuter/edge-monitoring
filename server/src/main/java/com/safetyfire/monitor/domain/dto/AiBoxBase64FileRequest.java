package com.safetyfire.monitor.domain.dto;

/**
 * AI盒子上传图片/录像：Base64 方式（兼容“接收到的是字符串”的场景）。
 */
public record AiBoxBase64FileRequest(
        String deviceSerial,
        Long alarmActionId,
        String originalUrl,
        String fileName,
        String contentType,
        String base64
) {
}

