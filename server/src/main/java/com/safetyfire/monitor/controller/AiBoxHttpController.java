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
import com.safetyfire.monitor.service.IngestService;
import com.safetyfire.monitor.util.AiBoxBase64;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.validation.Valid;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import jakarta.servlet.http.Part;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.LinkedHashMap;
import java.util.Map;
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
    private final IngestService ingestService;

    public AiBoxHttpController(AiBoxHttpPushService aiBoxHttpPushService,
                               HardwareIngestLogService hardwareIngestLogService,
                               ObjectMapper objectMapper,
                               IngestService ingestService) {
        this.aiBoxHttpPushService = aiBoxHttpPushService;
        this.hardwareIngestLogService = hardwareIngestLogService;
        this.objectMapper = objectMapper;
        this.ingestService = ingestService;
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

    /**
     * 兼容厂商/摄像头平台“只 POST 到服务器前缀”的推送方式（例如 POST /api/school/box）。
     *
     * 你提供的 logs 截图表明对方的推送请求为 multipart/form-data，字段包含 channel_num/user_name 等，
     * 且未拼接 /device/* 子路径。此处做兼容接收：
     * - 留痕：写入 hardware_ingest_log（topic=真实 URI）
     * - 文件：若包含图片/视频 Part，则落盘 + 入库 file_object/ai_box_media
     */
    @PostMapping(value = {"", "/"}, consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
    public AiBoxSimpleResponse vendorMultipart(HttpServletRequest request) {
        String uri = request.getRequestURI();
        Map<String, String> fields = new LinkedHashMap<>();
        int fileCount = 0;
        try {
            for (Part part : request.getParts()) {
                if (part.getSubmittedFileName() == null) {
                    String v = readPartAsString(part, 10_000);
                    fields.put(part.getName(), v);
                    continue;
                }
                fileCount++;
                String filename = StrUtil.blankToDefault(part.getSubmittedFileName(), part.getName());
                String ct = part.getContentType();
                String mediaType = isLikelyVideo(filename, ct) ? "VIDEO" : "IMAGE";
                byte[] bytes = readPartAsBytes(part, 25 * 1024 * 1024L);
                String deviceSerial = pickDeviceSerial(fields);
                aiBoxHttpPushService.ingestMedia(deviceSerial, null, mediaType, null, filename, ct, bytes);
            }

            // 如果对方不是文件 Part，而是 base64 字符串字段，也尝试落盘（兼容“接收到的是字符串”）
            String b64 = pickBase64(fields);
            if (fileCount == 0 && b64 != null && !b64.isBlank()) {
                byte[] bytes = AiBoxBase64.decode(b64);
                String deviceSerial = pickDeviceSerial(fields);
                aiBoxHttpPushService.ingestMedia(deviceSerial, null, "IMAGE", null, "image.jpg", "image/jpeg", bytes);
            }

            String deviceSerial = pickDeviceSerial(fields);
            String companyCode = ingestService.tryResolveCompanyCodeByDeviceOrCamera(deviceSerial);
            hardwareIngestLogService.recordStrict("HTTP", uri, "vendor-multipart", null, companyCode, safeJson(fields), true, null);
            return AiBoxSimpleResponse.ok();
        } catch (Exception e) {
            try {
                String deviceSerial = pickDeviceSerial(fields);
                String companyCode = ingestService.tryResolveCompanyCodeByDeviceOrCamera(deviceSerial);
                hardwareIngestLogService.record("HTTP", uri, "vendor-multipart", null, companyCode, safeJson(fields), false, e.getMessage());
            } catch (Exception ignore) {
            }
            return AiBoxSimpleResponse.fail(e.getMessage());
        }
    }

    @PostMapping(value = {"", "/"}, consumes = MediaType.APPLICATION_JSON_VALUE)
    public AiBoxSimpleResponse vendorJson(HttpServletRequest request, @RequestBody(required = false) JsonNode body) {
        String uri = request.getRequestURI();
        String raw = body == null ? "{}" : body.toString();
        try {
            // 仅留痕；如果包含 base64 字段则落盘
            String deviceSerial = pickDeviceSerial(body);
            String b64 = pickBase64(body);
            if (b64 != null && !b64.isBlank()) {
                byte[] bytes = AiBoxBase64.decode(b64);
                aiBoxHttpPushService.ingestMedia(deviceSerial, null, "IMAGE", null, "image.jpg", "image/jpeg", bytes);
            }
            String companyCode = ingestService.tryResolveCompanyCodeByDeviceOrCamera(deviceSerial);
            hardwareIngestLogService.recordStrict("HTTP", uri, "vendor-json", null, companyCode, raw, true, null);
            return AiBoxSimpleResponse.ok();
        } catch (Exception e) {
            try {
                String deviceSerial = pickDeviceSerial(body);
                String companyCode = ingestService.tryResolveCompanyCodeByDeviceOrCamera(deviceSerial);
                hardwareIngestLogService.record("HTTP", uri, "vendor-json", null, companyCode, raw, false, e.getMessage());
            } catch (Exception ignore) {
            }
            return AiBoxSimpleResponse.fail(e.getMessage());
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

    private static String readPartAsString(Part part, int maxChars) throws Exception {
        try (InputStream in = part.getInputStream()) {
            byte[] bytes = in.readNBytes(Math.max(0, maxChars) * 4L > Integer.MAX_VALUE ? Integer.MAX_VALUE : maxChars * 4);
            String s = new String(bytes, StandardCharsets.UTF_8);
            if (s.length() <= maxChars) return s;
            return s.substring(0, maxChars);
        }
    }

    private static byte[] readPartAsBytes(Part part, long maxBytes) throws Exception {
        long size = part.getSize();
        if (size > maxBytes) {
            throw new IllegalArgumentException("文件过大，size=" + size);
        }
        try (InputStream in = part.getInputStream()) {
            return in.readAllBytes();
        }
    }

    private static boolean isLikelyVideo(String filename, String contentType) {
        String ct = contentType == null ? "" : contentType.toLowerCase();
        if (ct.startsWith("video/")) return true;
        String f = filename == null ? "" : filename.toLowerCase();
        return f.endsWith(".mp4") || f.endsWith(".avi") || f.endsWith(".mov") || f.endsWith(".flv") || f.endsWith(".mkv");
    }

    private static String pickDeviceSerial(Map<String, String> fields) {
        if (fields == null || fields.isEmpty()) return null;
        String[] keys = new String[]{
                "deviceSerial", "device_serial",
                "deviceId", "device_id",
                "device_uuid", "uuid",
                "serial", "sn",
                "serialno",
                "cameraCode", "camera_code",
                "channel_num", "channel"
        };
        for (String k : keys) {
            String v = fields.get(k);
            if (v != null && !v.isBlank()) return v.trim();
        }
        // 兜底：有些平台会用 user_name/用户名也作为维度，但这里不强行绑定
        return null;
    }

    private static String pickDeviceSerial(JsonNode body) {
        if (body == null || body.isNull()) return null;
        String[] keys = new String[]{
                "deviceSerial", "device_serial",
                "deviceId", "device_id",
                "uuid", "serial", "sn",
                "cameraCode", "camera_code",
                "channel_num", "channel"
        };
        for (String k : keys) {
            JsonNode v = body.get(k);
            if (v == null || v.isNull()) continue;
            String s = v.asText(null);
            if (s != null && !s.isBlank()) return s.trim();
        }
        return null;
    }

    private static String pickBase64(Map<String, String> fields) {
        if (fields == null || fields.isEmpty()) return null;
        String[] keys = new String[]{"base64", "imageBase64", "imgBase64", "picBase64", "picture", "image", "file", "data"};
        for (String k : keys) {
            String v = fields.get(k);
            if (v != null && v.length() > 80) return v;
        }
        return null;
    }

    private static String pickBase64(JsonNode body) {
        if (body == null || body.isNull()) return null;
        String[] keys = new String[]{"base64", "imageBase64", "imgBase64", "picBase64", "picture", "image", "data"};
        for (String k : keys) {
            JsonNode v = body.get(k);
            if (v == null || v.isNull()) continue;
            if (!v.isTextual()) continue;
            String s = v.asText();
            if (s != null && s.length() > 80) return s;
        }
        return null;
    }
}
