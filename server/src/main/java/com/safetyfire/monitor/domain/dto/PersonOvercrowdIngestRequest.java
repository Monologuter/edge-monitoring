package com.safetyfire.monitor.domain.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;

/**
 * 人员定位系统超员报警上报。
 */
public record PersonOvercrowdIngestRequest(
        @NotBlank(message = "区域编码不能为空") String areaCode,
        @NotBlank(message = "区域名称不能为空") String areaName,
        @NotNull(message = "当前人数不能为空") Integer currentCount,
        @NotNull(message = "最大人数不能为空") Integer maxCount,
        @NotNull(message = "时间戳不能为空") Long warningTime,
        String companyCode
) {
}
