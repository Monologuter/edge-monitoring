package com.safetyfire.monitor.controller;

import cn.hutool.core.util.StrUtil;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.safetyfire.monitor.domain.dto.AiBoxBase64FileRequest;
import com.safetyfire.monitor.domain.dto.AiBoxHeartbeatRequest;
import com.safetyfire.monitor.domain.dto.AiBoxLoginRequest;
import com.safetyfire.monitor.domain.vo.AiBoxLoginResponse;
import com.safetyfire.monitor.domain.vo.AiBoxSimpleResponse;
import com.safetyfire.monitor.domain.vo.AiBoxUploadResponse;
import com.safetyfire.monitor.service.AiBoxHttpPushService;
import com.safetyfire.monitor.service.HardwareIngestLogService;
import com.safetyfire.monitor.util.AiBoxBase64;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.validation.Valid;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.util.Objects;

/**
 * AI边缘盒子 HTTP 对接（兼容文档中的 /device/*）。
 *
 * 说明：
 * - 文档中“服务器地址”是一个前缀，因此这里额外兼容了常见前缀：""、"/box"、"/api/school/box"
 * - 返回格式遵循文档示例：{code:"200", message:"success", ...}
 */
@RestController
@RequestMapping({"", "/box", "/api/school/box"})
public class AiBoxHttpController {
    private final AiBoxHttpPushService aiBoxHttpPushService;
    private final HardwareIngestLogService hardwareIngestLogService;
    private final ObjectMapper objectMapper;

    public AiBoxHttpController(AiBoxHttpPushService aiBoxHttpPushService,
                               HardwareIngestLogService hardwareIngestLogService,
                               ObjectMapper objectMapper) {
        this.aiBoxHttpPushService = aiBoxHttpPushService;
        this.hardwareIngestLogService = hardwareIngestLogService;
        this.objectMapper = objectMapper;
    }

    @PostMapping("/device/login")
    public AiBoxLoginResponse login(HttpServletRequest request, @Valid @RequestBody AiBoxLoginRequest req) {
        String body = safeJson(req);
        try {
            String token = aiBoxHttpPushService.login(req.deviceSerial(), clientIp(request));
            hardwareIngestLogService.record("HTTP", "/device/login", "ai-box-login", null, null, body, true, null);
            return AiBoxLoginResponse.ok(token);
        } catch (Exception e) {
            hardwareIngestLogService.record("HTTP", "/device/login", "ai-box-login", null, null, body, false, e.getMessage());
            return new AiBoxLoginResponse("500", Objects.requireNonNullElse(e.getMessage(), "error"), null);
        }
    }

    @PostMapping("/device/heartBeat")
    public AiBoxSimpleResponse heartBeat(HttpServletRequest request, @RequestBody(required = false) AiBoxHeartbeatRequest req) {
        String body = safeJson(req);
        try {
            aiBoxHttpPushService.heartbeat(req, clientIp(request));
            hardwareIngestLogService.record("HTTP", "/device/heartBeat", "ai-box-heartbeat", null, null, body, true, null);
            return AiBoxSimpleResponse.ok();
        } catch (Exception e) {
            hardwareIngestLogService.record("HTTP", "/device/heartBeat", "ai-box-heartbeat", null, null, body, false, e.getMessage());
            return AiBoxSimpleResponse.fail(e.getMessage());
        }
    }

    @PostMapping(value = "/device/alarm/action", consumes = MediaType.APPLICATION_JSON_VALUE)
    public AiBoxSimpleResponse alarmAction(@RequestBody(required = false) JsonNode body) {
        String raw = body == null ? "{}" : body.toString();
        try {
            aiBoxHttpPushService.ingestAlarmAction(raw);
            hardwareIngestLogService.record("HTTP", "/device/alarm/action", "ai-box-alarm-action", null, null, raw, true, null);
            return AiBoxSimpleResponse.ok();
        } catch (Exception e) {
            hardwareIngestLogService.record("HTTP", "/device/alarm/action", "ai-box-alarm-action", null, null, raw, false, e.getMessage());
            return AiBoxSimpleResponse.fail(e.getMessage());
        }
    }

    /**
     * 图片上传：兼容 multipart/form-data 与 base64 JSON 两种方式。
     */
    @PostMapping(value = "/device/image", consumes = {MediaType.MULTIPART_FORM_DATA_VALUE})
    public AiBoxUploadResponse imageMultipart(
            @RequestParam(required = false) String deviceSerial,
            @RequestParam(required = false) Long alarmActionId,
            @RequestParam(required = false) String originalUrl,
            @RequestPart(required = false) MultipartFile file,
            @RequestPart(required = false, name = "image") MultipartFile image
    ) {
        MultipartFile f = file != null ? file : image;
        if (f == null || f.isEmpty()) return new AiBoxUploadResponse("400", "file is empty", null);
        try {
            var stored = aiBoxHttpPushService.ingestMedia(
                    deviceSerial, alarmActionId, "IMAGE", originalUrl,
                    StrUtil.blankToDefault(f.getOriginalFilename(), "image.jpg"),
                    f.getContentType(),
                    f.getBytes()
            );
            hardwareIngestLogService.record("HTTP", "/device/image", "ai-box-image", null, null,
                    "{\"deviceSerial\":\"" + safe(deviceSerial) + "\",\"alarmActionId\":" + alarmActionId + "}", true, null);
            return AiBoxUploadResponse.ok(stored.publicUrl());
        } catch (Exception e) {
            hardwareIngestLogService.record("HTTP", "/device/image", "ai-box-image", null, null,
                    "{\"deviceSerial\":\"" + safe(deviceSerial) + "\",\"alarmActionId\":" + alarmActionId + "}", false, e.getMessage());
            return new AiBoxUploadResponse("500", Objects.requireNonNullElse(e.getMessage(), "error"), null);
        }
    }

    @PostMapping(value = "/device/image", consumes = {MediaType.APPLICATION_JSON_VALUE})
    public AiBoxUploadResponse imageBase64Json(@RequestBody(required = false) JsonNode body) {
        AiBoxBase64FileRequest req = normalizeBase64Body(body);
        String payload = safeJson(req);
        try {
            byte[] bytes = AiBoxBase64.decode(req.base64());
            var stored = aiBoxHttpPushService.ingestMedia(
                    req.deviceSerial(), req.alarmActionId(), "IMAGE", req.originalUrl(),
                    StrUtil.blankToDefault(req.fileName(), "image.jpg"),
                    req.contentType(),
                    bytes
            );
            hardwareIngestLogService.record("HTTP", "/device/image", "ai-box-image", null, null, payload, true, null);
            return AiBoxUploadResponse.ok(stored.publicUrl());
        } catch (Exception e) {
            hardwareIngestLogService.record("HTTP", "/device/image", "ai-box-image", null, null, payload, false, e.getMessage());
            return new AiBoxUploadResponse("500", Objects.requireNonNullElse(e.getMessage(), "error"), null);
        }
    }

    @PostMapping(value = "/device/image", consumes = {MediaType.TEXT_PLAIN_VALUE})
    public AiBoxUploadResponse imageBase64Text(@RequestBody(required = false) String base64) {
        AiBoxBase64FileRequest req = new AiBoxBase64FileRequest(null, null, null, null, null, base64);
        String payload = safeJson(req);
        try {
            byte[] bytes = AiBoxBase64.decode(req.base64());
            var stored = aiBoxHttpPushService.ingestMedia(
                    req.deviceSerial(), req.alarmActionId(), "IMAGE", req.originalUrl(),
                    StrUtil.blankToDefault(req.fileName(), "image.jpg"),
                    req.contentType(),
                    bytes
            );
            hardwareIngestLogService.record("HTTP", "/device/image", "ai-box-image", null, null, payload, true, null);
            return AiBoxUploadResponse.ok(stored.publicUrl());
        } catch (Exception e) {
            hardwareIngestLogService.record("HTTP", "/device/image", "ai-box-image", null, null, payload, false, e.getMessage());
            return new AiBoxUploadResponse("500", Objects.requireNonNullElse(e.getMessage(), "error"), null);
        }
    }

    /**
     * 录像上传：兼容 multipart/form-data 与 base64 JSON 两种方式。
     */
    @PostMapping(value = "/device/video", consumes = {MediaType.MULTIPART_FORM_DATA_VALUE})
    public AiBoxUploadResponse videoMultipart(
            @RequestParam(required = false) String deviceSerial,
            @RequestParam(required = false) Long alarmActionId,
            @RequestParam(required = false) String originalUrl,
            @RequestPart(required = false) MultipartFile file,
            @RequestPart(required = false, name = "video") MultipartFile video
    ) {
        MultipartFile f = file != null ? file : video;
        if (f == null || f.isEmpty()) return new AiBoxUploadResponse("400", "file is empty", null);
        try {
            var stored = aiBoxHttpPushService.ingestMedia(
                    deviceSerial, alarmActionId, "VIDEO", originalUrl,
                    StrUtil.blankToDefault(f.getOriginalFilename(), "video.mp4"),
                    f.getContentType(),
                    f.getBytes()
            );
            hardwareIngestLogService.record("HTTP", "/device/video", "ai-box-video", null, null,
                    "{\"deviceSerial\":\"" + safe(deviceSerial) + "\",\"alarmActionId\":" + alarmActionId + "}", true, null);
            return AiBoxUploadResponse.ok(stored.publicUrl());
        } catch (Exception e) {
            hardwareIngestLogService.record("HTTP", "/device/video", "ai-box-video", null, null,
                    "{\"deviceSerial\":\"" + safe(deviceSerial) + "\",\"alarmActionId\":" + alarmActionId + "}", false, e.getMessage());
            return new AiBoxUploadResponse("500", Objects.requireNonNullElse(e.getMessage(), "error"), null);
        }
    }

    @PostMapping(value = "/device/video", consumes = {MediaType.APPLICATION_JSON_VALUE})
    public AiBoxUploadResponse videoBase64Json(@RequestBody(required = false) JsonNode body) {
        AiBoxBase64FileRequest req = normalizeBase64Body(body);
        String payload = safeJson(req);
        try {
            byte[] bytes = AiBoxBase64.decode(req.base64());
            var stored = aiBoxHttpPushService.ingestMedia(
                    req.deviceSerial(), req.alarmActionId(), "VIDEO", req.originalUrl(),
                    StrUtil.blankToDefault(req.fileName(), "video.mp4"),
                    req.contentType(),
                    bytes
            );
            hardwareIngestLogService.record("HTTP", "/device/video", "ai-box-video", null, null, payload, true, null);
            return AiBoxUploadResponse.ok(stored.publicUrl());
        } catch (Exception e) {
            hardwareIngestLogService.record("HTTP", "/device/video", "ai-box-video", null, null, payload, false, e.getMessage());
            return new AiBoxUploadResponse("500", Objects.requireNonNullElse(e.getMessage(), "error"), null);
        }
    }

    @PostMapping(value = "/device/video", consumes = {MediaType.TEXT_PLAIN_VALUE})
    public AiBoxUploadResponse videoBase64Text(@RequestBody(required = false) String base64) {
        AiBoxBase64FileRequest req = new AiBoxBase64FileRequest(null, null, null, null, null, base64);
        String payload = safeJson(req);
        try {
            byte[] bytes = AiBoxBase64.decode(req.base64());
            var stored = aiBoxHttpPushService.ingestMedia(
                    req.deviceSerial(), req.alarmActionId(), "VIDEO", req.originalUrl(),
                    StrUtil.blankToDefault(req.fileName(), "video.mp4"),
                    req.contentType(),
                    bytes
            );
            hardwareIngestLogService.record("HTTP", "/device/video", "ai-box-video", null, null, payload, true, null);
            return AiBoxUploadResponse.ok(stored.publicUrl());
        } catch (Exception e) {
            hardwareIngestLogService.record("HTTP", "/device/video", "ai-box-video", null, null, payload, false, e.getMessage());
            return new AiBoxUploadResponse("500", Objects.requireNonNullElse(e.getMessage(), "error"), null);
        }
    }

    private String clientIp(HttpServletRequest request) {
        String xff = request.getHeader("X-Forwarded-For");
        if (xff != null && !xff.isBlank()) return xff.split(",")[0].trim();
        return request.getRemoteAddr();
    }

    private String safeJson(Object o) {
        try {
            return objectMapper.writeValueAsString(o);
        } catch (Exception e) {
            return String.valueOf(o);
        }
    }

    private static String safe(String s) {
        return s == null ? "" : s.replace("\"", "");
    }

    private AiBoxBase64FileRequest normalizeBase64Body(JsonNode body) {
        if (body == null || body.isNull()) return new AiBoxBase64FileRequest(null, null, null, null, null, null);
        if (body.isTextual()) {
            return new AiBoxBase64FileRequest(null, null, null, null, null, body.asText());
        }
        try {
            String base64 = text(body, "base64", "data", "file", "content");
            String fileName = text(body, "fileName", "filename", "name");
            String ct = text(body, "contentType", "content_type", "mime");
            String deviceSerial = text(body, "deviceSerial", "device_serial");
            String originalUrl = text(body, "originalUrl", "url", "fileUrl", "file_url");
            Long alarmActionId = longFirst(body, "alarmActionId", "alarmId", "alarm_action_id");
            return new AiBoxBase64FileRequest(deviceSerial, alarmActionId, originalUrl, fileName, ct, base64);
        } catch (Exception ignore) {
        }
        return new AiBoxBase64FileRequest(null, null, null, null, null, body.toString());
    }

    private static String text(JsonNode n, String... keys) {
        if (n == null) return null;
        for (String k : keys) {
            JsonNode v = n.get(k);
            if (v == null || v.isNull()) continue;
            String s = v.asText(null);
            if (s != null && !s.isBlank()) return s.trim();
        }
        return null;
    }

    private static Long longFirst(JsonNode n, String... keys) {
        if (n == null) return null;
        for (String k : keys) {
            JsonNode v = n.get(k);
            if (v == null || v.isNull()) continue;
            if (v.isNumber()) return v.longValue();
            String s = v.asText(null);
            if (s == null || s.isBlank()) continue;
            try {
                return Long.parseLong(s.trim());
            } catch (NumberFormatException ignore) {
            }
        }
        return null;
    }
}
