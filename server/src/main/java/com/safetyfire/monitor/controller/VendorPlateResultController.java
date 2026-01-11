package com.safetyfire.monitor.controller;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.safetyfire.monitor.domain.vo.AiBoxSimpleResponse;
import com.safetyfire.monitor.service.HardwareIngestLogService;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.*;

/**
 * 兼容部分摄像头/平台推送：/devicemanagement/php/plateresult.php
 *
 * 你提供的 logs 截图显示设备会向该路径 POST application/json；
 * 若不兼容会返回 403（未放行）或 404/500（未实现），导致“看似推送成功但实际无入库”。
 *
 * 处理策略：
 * - 原始 JSON 入库留痕（hardware_ingest_log），便于你后续按字段对齐业务入库
 * - 返回 200 + {code/message}，避免厂商误判失败后停止推送
 */
@RestController
@RequestMapping("/devicemanagement/php")
public class VendorPlateResultController {
    private final HardwareIngestLogService hardwareIngestLogService;
    private final ObjectMapper objectMapper;

    public VendorPlateResultController(HardwareIngestLogService hardwareIngestLogService, ObjectMapper objectMapper) {
        this.hardwareIngestLogService = hardwareIngestLogService;
        this.objectMapper = objectMapper;
    }

    @PostMapping(value = "/plateresult.php", consumes = MediaType.APPLICATION_JSON_VALUE)
    public AiBoxSimpleResponse plateResultJson(@RequestBody(required = false) JsonNode body) {
        String raw = body == null ? "{}" : body.toString();
        try {
            hardwareIngestLogService.record("HTTP", "/devicemanagement/php/plateresult.php", "vendor-plateresult", null, null, raw, true, null);
            return AiBoxSimpleResponse.ok();
        } catch (Exception e) {
            hardwareIngestLogService.record("HTTP", "/devicemanagement/php/plateresult.php", "vendor-plateresult", null, null, raw, false, e.getMessage());
            return AiBoxSimpleResponse.fail(e.getMessage());
        }
    }

    @PostMapping(value = "/plateresult.php", consumes = {MediaType.TEXT_PLAIN_VALUE, MediaType.APPLICATION_OCTET_STREAM_VALUE})
    public AiBoxSimpleResponse plateResultText(@RequestBody(required = false) String body) {
        String raw = body == null ? "" : body;
        try {
            // 尝试当 JSON 解析（避免某些设备 text/plain 传 JSON）
            try {
                objectMapper.readTree(raw);
            } catch (Exception ignore) {
            }
            hardwareIngestLogService.record("HTTP", "/devicemanagement/php/plateresult.php", "vendor-plateresult", null, null, raw, true, null);
            return AiBoxSimpleResponse.ok();
        } catch (Exception e) {
            hardwareIngestLogService.record("HTTP", "/devicemanagement/php/plateresult.php", "vendor-plateresult", null, null, raw, false, e.getMessage());
            return AiBoxSimpleResponse.fail(e.getMessage());
        }
    }
}

