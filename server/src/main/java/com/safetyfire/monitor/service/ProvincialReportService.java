package com.safetyfire.monitor.service;

import cn.hutool.json.JSONUtil;
import com.safetyfire.monitor.domain.dto.ProvincialFeedbackResponse;
import com.safetyfire.monitor.domain.dto.ProvincialReportRequest;
import com.safetyfire.monitor.domain.dto.ProvincialUnitInfoDTO;
import com.safetyfire.monitor.domain.dto.ProvincialWarningReportDTO;
import com.safetyfire.monitor.util.AesUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.*;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestClient;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 省厅数据上报服务。
 */
@Service
public class ProvincialReportService {
    private static final Logger log = LoggerFactory.getLogger(ProvincialReportService.class);

    private final boolean enabled;
    private final String baseUrl;
    private final String username;
    private final String password;
    private final String aesKey;
    private final String aesIv;
    private final RestClient restClient;

    public ProvincialReportService(
            @Value("${app.provincial.report.enabled:false}") boolean enabled,
            @Value("${app.provincial.report.base-url:http://localhost:9999}") String baseUrl,
            @Value("${app.provincial.report.username:53090200003}") String username,
            @Value("${app.provincial.report.password:uq,w9CcS1FYwT#8_}") String password,
            @Value("${app.provincial.report.aes-key:K5znJYEe+PzHKgBPgmj3zg==}") String aesKey,
            @Value("${app.provincial.report.aes-iv:XsQrfnKhHcwaxPIKAqK3Qw==}") String aesIv,
            RestClient.Builder restClientBuilder
    ) {
        this.enabled = enabled;
        this.baseUrl = baseUrl;
        this.username = username;
        this.password = password;
        this.aesKey = aesKey;
        this.aesIv = aesIv;
        this.restClient = restClientBuilder.baseUrl(baseUrl).build();
    }

    /**
     * 上报企业基础信息。
     */
    public boolean reportUnitInfo(ProvincialUnitInfoDTO unitInfo) {
        if (!enabled) {
            log.info("省厅上报未启用，跳过企业信息上报");
            return false;
        }

        try {
            // 构建请求体
            Map<String, Object> data = new HashMap<>();
            data.put("unitCode", unitInfo.getUnitCode());
            data.put("unitName", unitInfo.getUnitName());
            data.put("unitType", unitInfo.getUnitType());
            data.put("creditCode", unitInfo.getCreditCode());
            data.put("legalPerson", unitInfo.getLegalPerson());
            data.put("contactPerson", unitInfo.getContactPerson());
            data.put("contactPhone", unitInfo.getContactPhone());
            data.put("address", unitInfo.getAddress());
            data.put("longitude", unitInfo.getLongitude());
            data.put("latitude", unitInfo.getLatitude());
            data.put("areaCode", unitInfo.getAreaCode());
            data.put("businessLicense", unitInfo.getBusinessLicense());
            data.put("establishDate", unitInfo.getEstablishDate());
            data.put("registerCapital", unitInfo.getRegisterCapital());
            data.put("unitStatus", unitInfo.getUnitStatus());
            data.put("hazardCategories", unitInfo.getHazardCategories());
            data.put("maxCapacity", unitInfo.getMaxCapacity());
            data.put("emergencyPlan", unitInfo.getEmergencyPlan());
            data.put("emergencyContact", unitInfo.getEmergencyContact());
            data.put("emergencyPhone", unitInfo.getEmergencyPhone());

            return sendRequest("/safe/edu/reg/unit/unit/save", data);
        } catch (Exception e) {
            log.error("企业基础信息上报失败: {}", e.getMessage(), e);
            return false;
        }
    }

    /**
     * 上报危险品五停预警信息。
     */
    public boolean reportWarning(ProvincialWarningReportDTO warning) {
        if (!enabled) {
            log.info("省厅上报未启用，跳过预警信息上报");
            return false;
        }

        try {
            // 构建请求体
            Map<String, Object> data = new HashMap<>();
            data.put("warningId", warning.getWarningId());
            data.put("unitCode", warning.getUnitCode());
            data.put("unitName", warning.getUnitName());
            data.put("warningType", warning.getWarningType());
            data.put("warningLevel", warning.getWarningLevel());
            data.put("warningTime", warning.getWarningTime());
            data.put("warningContent", warning.getWarningContent());
            data.put("warningStatus", warning.getWarningStatus());
            data.put("handleResult", warning.getHandleResult());
            data.put("handlePerson", warning.getHandlePerson());
            data.put("handleTime", warning.getHandleTime());
            data.put("feedback", warning.getFeedback());
            data.put("longitude", warning.getLongitude());
            data.put("latitude", warning.getLatitude());
            data.put("deviceCode", warning.getDeviceCode());
            data.put("deviceName", warning.getDeviceName());
            data.put("hazardCategory", warning.getHazardCategory());
            data.put("capacity", warning.getCapacity());
            data.put("actualAmount", warning.getActualAmount());
            data.put("stopType", warning.getStopType());
            data.put("stopReason", warning.getStopReason());

            return sendRequest("/safe/edu/reg/enterprise/warning/save", data);
        } catch (Exception e) {
            log.error("预警信息上报失败: {}", e.getMessage(), e);
            return false;
        }
    }

    /**
     * 拉取预警反馈。
     */
    public List<ProvincialFeedbackResponse> fetchFeedback(String warningId) {
        if (!enabled) {
            log.info("省厅上报未启用，跳过反馈拉取");
            return List.of();
        }

        try {
            // 构建请求参数
            Map<String, Object> params = new HashMap<>();
            params.put("warningId", warningId);

            // 发送请求
            String response = restClient.get()
                    .uri("/safe/edu/reg/enterprise/warning/feedback", params)
                    .headers(headers -> {
                        headers.setContentType(MediaType.APPLICATION_JSON);
                    })
                    .retrieve()
                    .body(String.class);

            log.info("反馈拉取响应: {}", response);

            // 解析响应（根据实际响应格式调整）
            if (response != null && !response.isBlank()) {
                // 假设响应格式为 { "code": 200, "data": [...] }
                Map<String, Object> result = JSONUtil.toBean(response, Map.class);
                if (result != null && result.containsKey("data")) {
                    List<Map<String, Object>> dataList = (List<Map<String, Object>>) result.get("data");
                    return dataList.stream().map(item -> {
                        ProvincialFeedbackResponse feedback = new ProvincialFeedbackResponse();
                        feedback.setWarningId((String) item.get("warningId"));
                        feedback.setFeedback((String) item.get("feedback"));
                        feedback.setFeedbackTime((String) item.get("feedbackTime"));
                        feedback.setFeedbackPerson((String) item.get("feedbackPerson"));
                        feedback.setHandleResult((String) item.get("handleResult"));
                        return feedback;
                    }).toList();
                }
            }

            return List.of();
        } catch (Exception e) {
            log.error("预警反馈拉取失败: {}", e.getMessage(), e);
            return List.of();
        }
    }

    /**
     * 发送加密请求到省厅系统。
     */
    private boolean sendRequest(String path, Map<String, Object> data) {
        try {
            // 1. 构建加密前的请求对象
            ProvincialReportRequest request = new ProvincialReportRequest();
            request.setUsername(username);
            request.setPassword(password);
            request.setData(JSONUtil.toJsonStr(data));
            request.setTimestamp(System.currentTimeMillis());

            // 2. 加密整个请求体
            String plainJson = JSONUtil.toJsonStr(request);
            String encryptedData = AesUtil.encrypt(plainJson, aesKey, aesIv);

            // 3. 构建最终请求
            Map<String, String> finalRequest = new HashMap<>();
            finalRequest.put("data", encryptedData);

            // 4. 发送请求
            String response = restClient.post()
                    .uri(path)
                    .contentType(MediaType.APPLICATION_JSON)
                    .body(finalRequest)
                    .retrieve()
                    .body(String.class);

            log.info("省厅上报响应: path={}, response={}", path, response);

            // 5. 解析响应
            if (response != null && !response.isBlank()) {
                Map<String, Object> result = JSONUtil.toBean(response, Map.class);
                if (result != null) {
                    Object code = result.get("code");
                    if (code != null && "200".equals(code.toString())) {
                        return true;
                    }
                }
            }

            return false;
        } catch (Exception e) {
            log.error("省厅上报失败: path={}, error={}", path, e.getMessage(), e);
            return false;
        }
    }
}
