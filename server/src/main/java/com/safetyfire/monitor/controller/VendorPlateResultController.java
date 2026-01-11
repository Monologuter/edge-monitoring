package com.safetyfire.monitor.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.safetyfire.monitor.domain.vo.AiBoxSimpleResponse;
import com.safetyfire.monitor.service.HardwareIngestLogService;
import com.safetyfire.monitor.service.IngestService;
import jakarta.servlet.http.HttpServletRequest;
import org.springframework.web.bind.annotation.*;

import java.nio.charset.StandardCharsets;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

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
    private final IngestService ingestService;

    private static final Pattern SERIALNO = Pattern.compile("serialno\\s*[:=]\\s*([A-Za-z0-9\\-_.]+)", Pattern.CASE_INSENSITIVE);

    public VendorPlateResultController(HardwareIngestLogService hardwareIngestLogService, ObjectMapper objectMapper,
                                       IngestService ingestService) {
        this.hardwareIngestLogService = hardwareIngestLogService;
        this.objectMapper = objectMapper;
        this.ingestService = ingestService;
    }

    /**
     * 设备端 Content-Type 经常出现大小写不规范（如 application/Json），甚至 body 不是标准 JSON。
     * 这里不强依赖 MessageConverter 的 JSON 解析，直接按字节读取并留痕入库。
     */
    @PostMapping("/plateresult.php")
    public AiBoxSimpleResponse plateResult(HttpServletRequest request, @RequestBody(required = false) byte[] bodyBytes) {
        String raw = bodyBytes == null ? "" : new String(bodyBytes, StandardCharsets.UTF_8);
        try {
            // 尝试当 JSON 解析（若失败也不影响留痕）
            try {
                if (!raw.isBlank()) objectMapper.readTree(raw);
            } catch (Exception ignore) {
            }
            String deviceOrCameraCode = tryExtractSerialNo(raw);
            String companyCode = ingestService.tryResolveCompanyCodeByDeviceOrCamera(deviceOrCameraCode);
            hardwareIngestLogService.recordStrict("HTTP", request.getRequestURI(), "vendor-plateresult",
                    null, companyCode, raw, true, null);
            return AiBoxSimpleResponse.ok();
        } catch (Exception e) {
            hardwareIngestLogService.record("HTTP", request.getRequestURI(), "vendor-plateresult",
                    null, null, raw, false, e.getMessage());
            return AiBoxSimpleResponse.fail(e.getMessage());
        }
    }

    private static String tryExtractSerialNo(String raw) {
        if (raw == null) return null;
        Matcher m = SERIALNO.matcher(raw);
        if (m.find()) return m.group(1);
        return null;
    }
}
