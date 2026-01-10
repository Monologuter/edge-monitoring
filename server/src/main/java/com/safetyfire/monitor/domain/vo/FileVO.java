package com.safetyfire.monitor.domain.vo;

/**
 * 文件上传返回对象。
 */
public record FileVO(Long id, String originalName, String contentType, long sizeBytes, String downloadUrl) {
}

