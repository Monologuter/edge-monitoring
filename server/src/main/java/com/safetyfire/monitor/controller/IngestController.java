package com.safetyfire.monitor.controller;

import com.safetyfire.monitor.common.ApiResponse;
import com.safetyfire.monitor.domain.dto.AiBoxAlarmIngestRequest;
import com.safetyfire.monitor.domain.dto.AlarmIngestRequest;
import com.safetyfire.monitor.domain.dto.CarInoutIngestRequest;
import com.safetyfire.monitor.domain.dto.DeviceReadingIngestRequest;
import com.safetyfire.monitor.domain.dto.PersonInoutIngestRequest;
import com.safetyfire.monitor.domain.dto.PersonOvercrowdIngestRequest;
import com.safetyfire.monitor.domain.dto.ServerHeartbeatIngestRequest;
import com.safetyfire.monitor.service.AiBoxAlarmService;
import com.safetyfire.monitor.service.CarInoutService;
import com.safetyfire.monitor.service.DeviceAuthService;
import com.safetyfire.monitor.service.HardwareIngestLogService;
import com.safetyfire.monitor.service.IngestService;
import com.safetyfire.monitor.service.PersonInoutService;
import com.safetyfire.monitor.service.PersonOvercrowdService;
import com.safetyfire.monitor.service.ServerHeartbeatService;
import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.*;

import jakarta.servlet.http.HttpServletRequest;

/**
 * 数据接入接口：给硬件/串口服务器/边缘设备调用。
 *
 * 说明：
 * - 此接口不走登录态 JWT，使用 X-Device-Api-Key 做设备鉴权；
 * - 生产可进一步增加签名与 IP 白名单。
 */
@RestController
@RequestMapping("/api/v1/ingest")
public class IngestController {
    private final DeviceAuthService deviceAuthService;
    private final IngestService ingestService;
    private final PersonInoutService personInoutService;
    private final CarInoutService carInoutService;
    private final ServerHeartbeatService serverHeartbeatService;
    private final AiBoxAlarmService aiBoxAlarmService;
    private final PersonOvercrowdService personOvercrowdService;
    private final HardwareIngestLogService hardwareIngestLogService;

    public IngestController(DeviceAuthService deviceAuthService, IngestService ingestService,
                            PersonInoutService personInoutService, CarInoutService carInoutService,
                            ServerHeartbeatService serverHeartbeatService, AiBoxAlarmService aiBoxAlarmService,
                            PersonOvercrowdService personOvercrowdService, HardwareIngestLogService hardwareIngestLogService) {
        this.deviceAuthService = deviceAuthService;
        this.ingestService = ingestService;
        this.personInoutService = personInoutService;
        this.carInoutService = carInoutService;
        this.serverHeartbeatService = serverHeartbeatService;
        this.aiBoxAlarmService = aiBoxAlarmService;
        this.personOvercrowdService = personOvercrowdService;
        this.hardwareIngestLogService = hardwareIngestLogService;
    }

    @PostMapping("/device-reading")
    public ApiResponse<Void> deviceReading(
            HttpServletRequest request,
            @RequestHeader(value = DeviceAuthService.HEADER, required = false) String apiKey,
            @RequestHeader(value = DeviceAuthService.HEADER_TS, required = false) String timestamp,
            @RequestHeader(value = DeviceAuthService.HEADER_NONCE, required = false) String nonce,
            @RequestHeader(value = DeviceAuthService.HEADER_SIG, required = false) String signature,
            @Valid @RequestBody DeviceReadingIngestRequest req
    ) {
        String body = cn.hutool.json.JSONUtil.toJsonStr(req);
        try {
            deviceAuthService.assertValid(request, apiKey, timestamp, nonce, signature);
            ingestService.ingestDeviceReading(req.deviceCode(), req.realValue(), req.systime());
            String companyCode = ingestService.tryResolveCompanyCodeByDeviceOrCamera(req.deviceCode());
            hardwareIngestLogService.record("HTTP", "/api/v1/ingest/device-reading", "device-reading", apiKey, companyCode, body, true, null);
            return ApiResponse.ok(null);
        } catch (Exception e) {
            hardwareIngestLogService.record("HTTP", "/api/v1/ingest/device-reading", "device-reading", apiKey, null, body, false, e.getMessage());
            throw e;
        }
    }

    @PostMapping("/alarm")
    public ApiResponse<Void> alarm(
            HttpServletRequest request,
            @RequestHeader(value = DeviceAuthService.HEADER, required = false) String apiKey,
            @RequestHeader(value = DeviceAuthService.HEADER_TS, required = false) String timestamp,
            @RequestHeader(value = DeviceAuthService.HEADER_NONCE, required = false) String nonce,
            @RequestHeader(value = DeviceAuthService.HEADER_SIG, required = false) String signature,
            @Valid @RequestBody AlarmIngestRequest req
    ) {
        String body = cn.hutool.json.JSONUtil.toJsonStr(req);
        try {
            deviceAuthService.assertValid(request, apiKey, timestamp, nonce, signature);
            ingestService.ingestAlarm(req.alarmType(), req.alarmStatus(), req.warningTime(), req.deviceCode(), req.alarmFile());
            String companyCode = ingestService.tryResolveCompanyCodeByDeviceOrCamera(req.deviceCode());
            hardwareIngestLogService.record("HTTP", "/api/v1/ingest/alarm", "alarm", apiKey, companyCode, body, true, null);
            return ApiResponse.ok(null);
        } catch (Exception e) {
            hardwareIngestLogService.record("HTTP", "/api/v1/ingest/alarm", "alarm", apiKey, null, body, false, e.getMessage());
            throw e;
        }
    }

    @PostMapping("/person-inout")
    public ApiResponse<Void> personInout(
            HttpServletRequest request,
            @RequestHeader(value = DeviceAuthService.HEADER, required = false) String apiKey,
            @RequestHeader(value = DeviceAuthService.HEADER_TS, required = false) String timestamp,
            @RequestHeader(value = DeviceAuthService.HEADER_NONCE, required = false) String nonce,
            @RequestHeader(value = DeviceAuthService.HEADER_SIG, required = false) String signature,
            @Valid @RequestBody PersonInoutIngestRequest req
    ) {
        String body = cn.hutool.json.JSONUtil.toJsonStr(req);
        try {
            deviceAuthService.assertValid(request, apiKey, timestamp, nonce, signature);
            String companyCode = personInoutService.ingest(req);
            hardwareIngestLogService.record("HTTP", "/api/v1/ingest/person-inout", "person-inout", apiKey, companyCode, body, true, null);
            return ApiResponse.ok(null);
        } catch (Exception e) {
            hardwareIngestLogService.record("HTTP", "/api/v1/ingest/person-inout", "person-inout", apiKey, null, body, false, e.getMessage());
            throw e;
        }
    }

    @PostMapping("/car-inout")
    public ApiResponse<Void> carInout(
            HttpServletRequest request,
            @RequestHeader(value = DeviceAuthService.HEADER, required = false) String apiKey,
            @RequestHeader(value = DeviceAuthService.HEADER_TS, required = false) String timestamp,
            @RequestHeader(value = DeviceAuthService.HEADER_NONCE, required = false) String nonce,
            @RequestHeader(value = DeviceAuthService.HEADER_SIG, required = false) String signature,
            @Valid @RequestBody CarInoutIngestRequest req
    ) {
        String body = cn.hutool.json.JSONUtil.toJsonStr(req);
        try {
            deviceAuthService.assertValid(request, apiKey, timestamp, nonce, signature);
            String companyCode = carInoutService.ingest(req);
            hardwareIngestLogService.record("HTTP", "/api/v1/ingest/car-inout", "car-inout", apiKey, companyCode, body, true, null);
            return ApiResponse.ok(null);
        } catch (Exception e) {
            hardwareIngestLogService.record("HTTP", "/api/v1/ingest/car-inout", "car-inout", apiKey, null, body, false, e.getMessage());
            throw e;
        }
    }

    @PostMapping("/server-heartbeat")
    public ApiResponse<Void> serverHeartbeat(
            HttpServletRequest request,
            @RequestHeader(value = DeviceAuthService.HEADER, required = false) String apiKey,
            @RequestHeader(value = DeviceAuthService.HEADER_TS, required = false) String timestamp,
            @RequestHeader(value = DeviceAuthService.HEADER_NONCE, required = false) String nonce,
            @RequestHeader(value = DeviceAuthService.HEADER_SIG, required = false) String signature,
            @Valid @RequestBody ServerHeartbeatIngestRequest req
    ) {
        String body = cn.hutool.json.JSONUtil.toJsonStr(req);
        try {
            deviceAuthService.assertValid(request, apiKey, timestamp, nonce, signature);
            serverHeartbeatService.ingest(req);
            hardwareIngestLogService.record("HTTP", "/api/v1/ingest/server-heartbeat", "server-heartbeat", apiKey, req.companyCode(), body, true, null);
            return ApiResponse.ok(null);
        } catch (Exception e) {
            hardwareIngestLogService.record("HTTP", "/api/v1/ingest/server-heartbeat", "server-heartbeat", apiKey, null, body, false, e.getMessage());
            throw e;
        }
    }

    /**
     * AI盒子告警上报接口（通道堵塞、超高超重、摄像头遮挡偏移等）。
     */
    @PostMapping("/ai-box-alarm")
    public ApiResponse<Void> aiBoxAlarm(
            HttpServletRequest request,
            @RequestHeader(value = DeviceAuthService.HEADER, required = false) String apiKey,
            @RequestHeader(value = DeviceAuthService.HEADER_TS, required = false) String timestamp,
            @RequestHeader(value = DeviceAuthService.HEADER_NONCE, required = false) String nonce,
            @RequestHeader(value = DeviceAuthService.HEADER_SIG, required = false) String signature,
            @Valid @RequestBody AiBoxAlarmIngestRequest req
    ) {
        String body = cn.hutool.json.JSONUtil.toJsonStr(req);
        try {
            deviceAuthService.assertValid(request, apiKey, timestamp, nonce, signature);
            String companyCode = aiBoxAlarmService.ingest(req);
            hardwareIngestLogService.record("HTTP", "/api/v1/ingest/ai-box-alarm", "ai-box-alarm", apiKey, companyCode, body, true, null);
            return ApiResponse.ok(null);
        } catch (Exception e) {
            hardwareIngestLogService.record("HTTP", "/api/v1/ingest/ai-box-alarm", "ai-box-alarm", apiKey, null, body, false, e.getMessage());
            throw e;
        }
    }

    /**
     * 人员定位系统超员报警上报接口。
     */
    @PostMapping("/person-overcrowd")
    public ApiResponse<Void> personOvercrowd(
            HttpServletRequest request,
            @RequestHeader(value = DeviceAuthService.HEADER, required = false) String apiKey,
            @RequestHeader(value = DeviceAuthService.HEADER_TS, required = false) String timestamp,
            @RequestHeader(value = DeviceAuthService.HEADER_NONCE, required = false) String nonce,
            @RequestHeader(value = DeviceAuthService.HEADER_SIG, required = false) String signature,
            @Valid @RequestBody PersonOvercrowdIngestRequest req
    ) {
        String body = cn.hutool.json.JSONUtil.toJsonStr(req);
        try {
            deviceAuthService.assertValid(request, apiKey, timestamp, nonce, signature);
            personOvercrowdService.ingest(req);
            hardwareIngestLogService.record("HTTP", "/api/v1/ingest/person-overcrowd", "person-overcrowd", apiKey, req.companyCode(), body, true, null);
            return ApiResponse.ok(null);
        } catch (Exception e) {
            hardwareIngestLogService.record("HTTP", "/api/v1/ingest/person-overcrowd", "person-overcrowd", apiKey, null, body, false, e.getMessage());
            throw e;
        }
    }
}
