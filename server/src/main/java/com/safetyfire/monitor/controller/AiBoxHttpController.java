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
import java.util.ArrayList;
import java.util.List;

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

            int imagesStored = 0;

            // 场景 A：multipart 中没有文件 Part，而是直接给 base64 字符串字段
            if (fileCount == 0) {
                String b64 = pickBase64(fields);
                if (b64 != null && !b64.isBlank()) {
                    byte[] bytes = AiBoxBase64.decode(b64);
                    String deviceSerial = pickDeviceSerial(fields);
                    aiBoxHttpPushService.ingestMedia(deviceSerial, null, "IMAGE", null, "image.jpg", "image/jpeg", bytes);
                    imagesStored++;
                }
            }

            // 场景 B：multipart 中某个字段是 JSON 字符串，图片 base64 在 imageFragmentFile 等字段里
            if (fileCount == 0) {
                String deviceSerial = pickDeviceSerial(fields);
                int imagesFound = 0;
                for (Map.Entry<String, String> entry : fields.entrySet()) {
                    String v = entry.getValue();
                    if (v == null) continue;
                    String s = v.trim();
                    if (s.length() < 2) continue;
                    if (!(s.startsWith("{") || s.startsWith("["))) continue;
                    JsonNode body;
                    try {
                        body = objectMapper.readTree(s);
                    } catch (Exception ignore) {
                        continue;
                    }
                    List<Base64Image> images = new ArrayList<>();
                    collectBase64Images(body, images, 8);
                    imagesFound += images.size();
                    for (int i = 0; i < images.size(); i++) {
                        Base64Image img = images.get(i);
                        if (img.base64 == null || img.base64.isBlank()) continue;
                        byte[] bytes = AiBoxBase64.decode(img.base64);
                        if (bytes == null || bytes.length == 0) continue;
                        String ct = guessImageContentType(img.base64, bytes);
                        String ext = ct != null && ct.contains("png") ? "png" : "jpg";
                        String filename = "vendor_" + safeKey(img.key) + "_" + i + "." + ext;
                        aiBoxHttpPushService.ingestMedia(deviceSerial, null, "IMAGE", null, filename, ct, bytes);
                        imagesStored++;
                    }
                }

                fields.put("_imagesFound", String.valueOf(imagesFound));
                fields.put("_imagesStored", String.valueOf(imagesStored));
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

    /**
     * 兼容厂商/摄像头平台 JSON 推送（Content-Type 可能大小写不规范）。
     *
     * 重要：你提供的 payload 中包含 imageFragmentFile（Base64 JPEG），这里会递归提取并落盘，
     * 写入 ai_box_media/file_object。
     */
    @PostMapping(value = {"", "/"}, consumes = "*/*")
    public AiBoxSimpleResponse vendorJson(HttpServletRequest request, @RequestBody(required = false) byte[] bodyBytes) {
        String uri = request.getRequestURI();
        String raw = bodyBytes == null ? "" : new String(bodyBytes, StandardCharsets.UTF_8);
        try {
            JsonNode body = null;
            try {
                body = (raw == null || raw.isBlank()) ? null : objectMapper.readTree(raw);
            } catch (Exception ignore) {
            }

            String deviceSerial = pickDeviceSerial(body);
            List<Base64Image> images = new ArrayList<>();
            if (body != null) {
                collectBase64Images(body, images, 8);
            } else {
                // 兜底：raw 如果是纯 base64，也尝试落盘
                String maybeBase64 = raw == null ? null : raw.trim();
                if (maybeBase64 != null && maybeBase64.length() > 200) {
                    images.add(new Base64Image("body", maybeBase64));
                }
            }

            int storedCount = 0;
            for (int i = 0; i < images.size(); i++) {
                Base64Image img = images.get(i);
                if (img.base64 == null || img.base64.isBlank()) continue;
                byte[] bytes = AiBoxBase64.decode(img.base64);
                if (bytes == null || bytes.length == 0) continue;
                String ct = guessImageContentType(img.base64, bytes);
                String ext = ct != null && ct.contains("png") ? "png" : "jpg";
                String filename = "vendor_" + safeKey(img.key) + "_" + i + "." + ext;
                aiBoxHttpPushService.ingestMedia(deviceSerial, null, "IMAGE", null, filename, ct, bytes);
                storedCount++;
            }

            Map<String, Object> debug = new LinkedHashMap<>();
            debug.put("contentType", request.getContentType());
            debug.put("deviceSerial", deviceSerial);
            debug.put("imagesFound", images.size());
            debug.put("imagesStored", storedCount);

            String companyCode = ingestService.tryResolveCompanyCodeByDeviceOrCamera(deviceSerial);
            hardwareIngestLogService.recordStrict("HTTP", uri, "vendor-json", null, companyCode, safeJson(debug), true, null);
            return AiBoxSimpleResponse.ok();
        } catch (Exception e) {
            try {
                hardwareIngestLogService.record("HTTP", uri, "vendor-json", null, null, raw, false, e.getMessage());
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
     * 图片上传（兼容）：
     * - multipart/form-data：字段名不确定（可能是 file/image/pic/...），因此使用 request.getParts() 兜底解析
     * - application/json：可能为标准 JSON 或设备端大小写不规范（application/Json）
     * - text/plain：body 直接是 base64 字符串
     */
    @PostMapping("/device/image")
    public AiBoxUploadResponse imageAny(HttpServletRequest request,
                                        @RequestParam(required = false) String deviceSerial,
                                        @RequestParam(required = false) Long alarmActionId,
                                        @RequestParam(required = false) String originalUrl,
                                        @RequestPart(required = false) MultipartFile file,
                                        @RequestPart(required = false, name = "image") MultipartFile image) {
        String uri = request.getRequestURI();
        try {
            UploadPayload payload = extractUploadPayload(request, deviceSerial, alarmActionId, originalUrl, file, image);
            if (payload.bytes == null || payload.bytes.length == 0) {
                hardwareIngestLogService.recordStrict("HTTP", uri, "ai-box-image", null,
                        ingestService.tryResolveCompanyCodeByDeviceOrCamera(payload.deviceSerial),
                        safeJson(payload.debug), false, "empty image payload");
                return new AiBoxUploadResponse("400", "file is empty", null);
            }

            var stored = aiBoxHttpPushService.ingestMedia(
                    payload.deviceSerial, payload.alarmActionId, "IMAGE", payload.originalUrl,
                    StrUtil.blankToDefault(payload.fileName, "image.jpg"),
                    payload.contentType,
                    payload.bytes
            );

            hardwareIngestLogService.recordStrict("HTTP", uri, "ai-box-image", null,
                    ingestService.tryResolveCompanyCodeByDeviceOrCamera(payload.deviceSerial),
                    safeJson(payload.debug), true, null);
            return AiBoxUploadResponse.ok(stored.publicUrl());
        } catch (Exception e) {
            try {
                hardwareIngestLogService.record("HTTP", uri, "ai-box-image", null, null,
                        "{\"err\":\"" + safe(e.getMessage()) + "\"}", false, e.getMessage());
            } catch (Exception ignore) {
            }
            return new AiBoxUploadResponse("500", Objects.requireNonNullElse(e.getMessage(), "error"), null);
        }
    }

    /**
     * 录像上传（兼容）：同图片上传。
     */
    @PostMapping("/device/video")
    public AiBoxUploadResponse videoAny(HttpServletRequest request,
                                        @RequestParam(required = false) String deviceSerial,
                                        @RequestParam(required = false) Long alarmActionId,
                                        @RequestParam(required = false) String originalUrl,
                                        @RequestPart(required = false) MultipartFile file,
                                        @RequestPart(required = false, name = "video") MultipartFile video) {
        String uri = request.getRequestURI();
        try {
            UploadPayload payload = extractUploadPayload(request, deviceSerial, alarmActionId, originalUrl, file, video);
            if (payload.bytes == null || payload.bytes.length == 0) {
                hardwareIngestLogService.recordStrict("HTTP", uri, "ai-box-video", null,
                        ingestService.tryResolveCompanyCodeByDeviceOrCamera(payload.deviceSerial),
                        safeJson(payload.debug), false, "empty video payload");
                return new AiBoxUploadResponse("400", "file is empty", null);
            }

            var stored = aiBoxHttpPushService.ingestMedia(
                    payload.deviceSerial, payload.alarmActionId, "VIDEO", payload.originalUrl,
                    StrUtil.blankToDefault(payload.fileName, "video.mp4"),
                    payload.contentType,
                    payload.bytes
            );

            hardwareIngestLogService.recordStrict("HTTP", uri, "ai-box-video", null,
                    ingestService.tryResolveCompanyCodeByDeviceOrCamera(payload.deviceSerial),
                    safeJson(payload.debug), true, null);
            return AiBoxUploadResponse.ok(stored.publicUrl());
        } catch (Exception e) {
            try {
                hardwareIngestLogService.record("HTTP", uri, "ai-box-video", null, null,
                        "{\"err\":\"" + safe(e.getMessage()) + "\"}", false, e.getMessage());
            } catch (Exception ignore) {
            }
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

    private UploadPayload extractUploadPayload(HttpServletRequest request,
                                               String deviceSerial,
                                               Long alarmActionId,
                                               String originalUrl,
                                               MultipartFile primary,
                                               MultipartFile secondary) throws Exception {
        Map<String, Object> debug = new LinkedHashMap<>();
        debug.put("contentType", request.getContentType());
        debug.put("deviceSerialParam", deviceSerial);
        debug.put("alarmActionIdParam", alarmActionId);
        debug.put("originalUrlParam", originalUrl);

        // 1) 优先使用 Spring 绑定到的 MultipartFile（file/image/video）
        MultipartFile f = primary != null ? primary : secondary;
        if (f != null && !f.isEmpty()) {
            debug.put("source", "multipart-bound");
            debug.put("fileField", f.getName());
            debug.put("fileName", f.getOriginalFilename());
            debug.put("fileSize", f.getSize());
            return new UploadPayload(
                    deviceSerial,
                    alarmActionId,
                    originalUrl,
                    f.getOriginalFilename(),
                    f.getContentType(),
                    f.getBytes(),
                    debug
            );
        }

        // 2) 兜底：遍历 request.getParts()，兼容设备端字段名不固定
        List<Map<String, Object>> partsDebug = new ArrayList<>();
        Map<String, String> textFields = new LinkedHashMap<>();
        try {
            for (Part part : request.getParts()) {
                Map<String, Object> pd = new LinkedHashMap<>();
                pd.put("name", part.getName());
                pd.put("filename", part.getSubmittedFileName());
                pd.put("size", part.getSize());
                pd.put("contentType", part.getContentType());
                partsDebug.add(pd);

                if (part.getSubmittedFileName() == null) {
                    textFields.put(part.getName(), readPartAsString(part, 10_000));
                    continue;
                }

                debug.put("source", "multipart-any-part");
                debug.put("fileField", part.getName());
                debug.put("fileName", part.getSubmittedFileName());
                debug.put("fileSize", part.getSize());

                String serial = pickDeviceSerial(textFields);
                Long aid = alarmActionId != null ? alarmActionId : tryParseLong(textFields.get("alarmActionId"));
                String ourl = originalUrl != null ? originalUrl : firstNonBlank(textFields, "originalUrl", "url", "fileUrl", "file_url");
                debug.put("textFields", textFields);
                debug.put("parts", partsDebug);
                return new UploadPayload(
                        firstNonBlank(deviceSerial, serial),
                        aid,
                        ourl,
                        part.getSubmittedFileName(),
                        part.getContentType(),
                        readPartAsBytes(part, 200 * 1024 * 1024L),
                        debug
                );
            }
        } catch (Exception e) {
            debug.put("partsReadError", e.getMessage());
        }
        debug.put("textFields", textFields);
        debug.put("parts", partsDebug);

        // 3) 如果没有文件 part，尝试从 multipart 字段里拿 base64
        String b64 = pickBase64(textFields);
        if (b64 != null && !b64.isBlank()) {
            debug.put("source", "multipart-base64");
            String serial = pickDeviceSerial(textFields);
            return new UploadPayload(
                    firstNonBlank(deviceSerial, serial),
                    alarmActionId,
                    originalUrl,
                    "image.jpg",
                    "image/jpeg",
                    AiBoxBase64.decode(b64),
                    debug
            );
        }

        // 4) 非 multipart：读取 body，可能是 base64 / json / 二进制
        byte[] bodyBytes = request.getInputStream().readAllBytes();
        debug.put("source", "raw-body");
        debug.put("bodySize", bodyBytes.length);
        String asText = null;
        try {
            asText = new String(bodyBytes, StandardCharsets.UTF_8).trim();
        } catch (Exception ignore) {
        }
        if (asText != null && !asText.isBlank()) {
            // 4.1) 纯 base64（text/plain）
            try {
                byte[] decoded = AiBoxBase64.decode(asText);
                if (decoded != null && decoded.length > 0) {
                    debug.put("bodyKind", "base64");
                    return new UploadPayload(deviceSerial, alarmActionId, originalUrl, "image.jpg", "image/jpeg", decoded, debug);
                }
            } catch (Exception ignore) {
            }
            // 4.2) JSON（可能 content-type 大小写不规范）
            try {
                JsonNode node = objectMapper.readTree(asText);
                AiBoxBase64FileRequest req = normalizeBase64Body(node);
                debug.put("bodyKind", "json");
                byte[] decoded = AiBoxBase64.decode(req.base64());
                return new UploadPayload(
                        firstNonBlank(deviceSerial, req.deviceSerial()),
                        alarmActionId != null ? alarmActionId : req.alarmActionId(),
                        originalUrl != null ? originalUrl : req.originalUrl(),
                        req.fileName(),
                        req.contentType(),
                        decoded,
                        debug
                );
            } catch (Exception ignore) {
            }
        }

        // 4.3) 兜底：把 body 当作二进制文件（部分设备可能直接推二进制）
        debug.put("bodyKind", "binary");
        return new UploadPayload(deviceSerial, alarmActionId, originalUrl, null, request.getContentType(), bodyBytes, debug);
    }

    private static String firstNonBlank(Map<String, String> map, String... keys) {
        if (map == null) return null;
        for (String k : keys) {
            String v = map.get(k);
            if (v != null && !v.isBlank()) return v.trim();
        }
        return null;
    }

    private static String firstNonBlank(String a, String b) {
        if (a != null && !a.isBlank()) return a;
        if (b != null && !b.isBlank()) return b;
        return null;
    }

    private static Long tryParseLong(String s) {
        if (s == null || s.isBlank()) return null;
        try {
            return Long.parseLong(s.trim());
        } catch (NumberFormatException ignore) {
            return null;
        }
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
                "serialno",
                "cameraCode", "camera_code",
                "channel_num", "channel"
        };
        // 先尝试顶层（性能更好）
        for (String k : keys) {
            JsonNode v = body.get(k);
            if (v == null || v.isNull()) continue;
            String s = v.asText(null);
            if (s != null && !s.isBlank()) return s.trim();
        }
        // 再递归全树查找（兼容 AlarmInfoPlate.serialno 这类嵌套结构）
        for (String k : keys) {
            String s = findFirstText(body, k, 8);
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

    private record UploadPayload(String deviceSerial, Long alarmActionId, String originalUrl,
                                 String fileName, String contentType, byte[] bytes, Map<String, Object> debug) {
    }

    private record Base64Image(String key, String base64) {
    }

    private static void collectBase64Images(JsonNode node, List<Base64Image> out, int max) {
        if (node == null || out.size() >= max) return;
        if (node.isObject()) {
            node.fields().forEachRemaining(e -> {
                if (out.size() >= max) return;
                String k = e.getKey();
                JsonNode v = e.getValue();
                if (v != null && v.isTextual() && looksLikeBase64ImageKey(k, v.asText())) {
                    out.add(new Base64Image(k, v.asText()));
                }
                collectBase64Images(v, out, max);
            });
            return;
        }
        if (node.isArray()) {
            for (JsonNode n : node) {
                if (out.size() >= max) break;
                collectBase64Images(n, out, max);
            }
        }
    }

    private static boolean looksLikeBase64ImageKey(String key, String value) {
        if (value == null) return false;
        String v = value.trim();
        if (v.length() < 200) return false;
        String k = key == null ? "" : key.toLowerCase();
        if (k.contains("imagefragmentfile")) return true;
        if (k.equals("base64") || k.endsWith("base64")) return true;
        if (k.contains("image") || k.contains("picture") || k.contains("snapshot")) {
            return v.startsWith("/9j/") || v.startsWith("iVBOR") || v.startsWith("data:image/");
        }
        return false;
    }

    private static String findFirstText(JsonNode node, String key, int maxDepth) {
        if (node == null || node.isNull() || maxDepth < 0 || key == null) return null;
        if (node.isObject()) {
            JsonNode direct = node.get(key);
            if (direct != null && direct.isTextual()) {
                String s = direct.asText();
                if (s != null && !s.isBlank()) return s;
            }
            var it = node.fields();
            while (it.hasNext()) {
                var e = it.next();
                String s = findFirstText(e.getValue(), key, maxDepth - 1);
                if (s != null && !s.isBlank()) return s;
            }
            return null;
        }
        if (node.isArray()) {
            for (JsonNode n : node) {
                String s = findFirstText(n, key, maxDepth - 1);
                if (s != null && !s.isBlank()) return s;
            }
        }
        return null;
    }

    private static String guessImageContentType(String base64, byte[] bytes) {
        if (base64 != null) {
            String s = base64.trim();
            if (s.startsWith("data:image/png")) return "image/png";
            if (s.startsWith("data:image/jpeg") || s.startsWith("data:image/jpg")) return "image/jpeg";
            if (s.startsWith("iVBOR")) return "image/png";
            if (s.startsWith("/9j/")) return "image/jpeg";
        }
        if (bytes != null && bytes.length >= 8) {
            // PNG: 89 50 4E 47 0D 0A 1A 0A
            if ((bytes[0] & 0xFF) == 0x89 && bytes[1] == 0x50 && bytes[2] == 0x4E && bytes[3] == 0x47) {
                return "image/png";
            }
            // JPEG: FF D8
            if ((bytes[0] & 0xFF) == 0xFF && (bytes[1] & 0xFF) == 0xD8) {
                return "image/jpeg";
            }
        }
        return "application/octet-stream";
    }

    private static String safeKey(String key) {
        if (key == null || key.isBlank()) return "img";
        return key.replaceAll("[^A-Za-z0-9_\\-]", "_");
    }
}
