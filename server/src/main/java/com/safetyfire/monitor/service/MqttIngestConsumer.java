package com.safetyfire.monitor.service;

import cn.hutool.core.util.StrUtil;
import cn.hutool.json.JSONUtil;
import com.safetyfire.monitor.domain.dto.*;
import org.eclipse.paho.client.mqttv3.*;
import org.eclipse.paho.client.mqttv3.persist.MemoryPersistence;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.nio.charset.StandardCharsets;

/**
 * MQTT 硬件接入消费者：订阅主题并入库（适配 MQTTX）。
 *
 * Topic 约定（前缀可配置 app.mqtt.ingest.topic-prefix）：
 * - {prefix}/device-reading
 *
 * 说明：按现场真实协议，仅温度/湿度/液位等“设备实时值”走 MQTT；其他硬件走 HTTP。
 */
@Component
public class MqttIngestConsumer implements DisposableBean, MqttCallbackExtended {
    private static final Logger log = LoggerFactory.getLogger(MqttIngestConsumer.class);

    private final boolean enabled;
    private final String brokerUrl;
    private final String username;
    private final String password;
    private final String clientIdPrefix;
    private final String topicPrefix;
    private final int qos;

    private final DeviceAuthService deviceAuthService;
    private final IngestService ingestService;
    private final HardwareIngestLogService hardwareIngestLogService;

    private volatile MqttClient client;

    public MqttIngestConsumer(
            @Value("${app.mqtt.ingest.enabled}") boolean enabled,
            @Value("${app.mqtt.broker-url}") String brokerUrl,
            @Value("${app.mqtt.username}") String username,
            @Value("${app.mqtt.password}") String password,
            @Value("${app.mqtt.ingest.client-id-prefix}") String clientIdPrefix,
            @Value("${app.mqtt.ingest.topic-prefix}") String topicPrefix,
            @Value("${app.mqtt.ingest.qos}") int qos,
            DeviceAuthService deviceAuthService,
            IngestService ingestService,
            HardwareIngestLogService hardwareIngestLogService
    ) {
        this.enabled = enabled;
        this.brokerUrl = brokerUrl;
        this.username = username;
        this.password = password;
        this.clientIdPrefix = clientIdPrefix;
        this.topicPrefix = topicPrefix;
        this.qos = qos;
        this.deviceAuthService = deviceAuthService;
        this.ingestService = ingestService;
        this.hardwareIngestLogService = hardwareIngestLogService;

        if (enabled) {
            connectAsync();
        }
    }

    private void connectAsync() {
        new Thread(() -> {
            try {
                ensureConnected();
            } catch (Exception e) {
                log.warn("MQTT 接入连接失败 brokerUrl={} err={}", brokerUrl, e.getMessage());
            }
        }, "mqtt-ingest-connector").start();
    }

    private synchronized void ensureConnected() throws MqttException {
        if (!enabled) return;
        if (client != null && client.isConnected()) return;

        String clientId = clientIdPrefix + "-" + System.currentTimeMillis();
        MqttClient c = new MqttClient(brokerUrl, clientId, new MemoryPersistence());
        c.setCallback(this);

        MqttConnectOptions opt = new MqttConnectOptions();
        opt.setAutomaticReconnect(true);
        opt.setCleanSession(true);
        opt.setConnectionTimeout(10);
        opt.setKeepAliveInterval(20);
        if (StrUtil.isNotBlank(username)) opt.setUserName(username);
        if (StrUtil.isNotBlank(password)) opt.setPassword(password.toCharArray());

        c.connect(opt);
        this.client = c;
        subscribeAll();
        log.info("MQTT 接入已启动 brokerUrl={} clientId={} topicPrefix={}", brokerUrl, clientId, topicPrefix);
    }

    private void subscribeAll() throws MqttException {
        if (client == null || !client.isConnected()) return;
        client.subscribe(topicPrefix + "/device-reading", qos);
    }

    @Override
    public void connectComplete(boolean reconnect, String serverURI) {
        try {
            subscribeAll();
        } catch (Exception e) {
            log.warn("MQTT 订阅失败 uri={} err={}", serverURI, e.getMessage());
        }
    }

    @Override
    public void connectionLost(Throwable cause) {
        log.warn("MQTT 连接断开 err={}", cause == null ? "" : cause.getMessage());
    }

    @Override
    public void messageArrived(String topic, MqttMessage message) {
        String payload = new String(message.getPayload(), StandardCharsets.UTF_8);
        String messageType = resolveMessageType(topic);
        String apiKey = null;
        try {
            MqttIngestEnvelope env = JSONUtil.toBean(payload, MqttIngestEnvelope.class);
            apiKey = env.apiKey();
            deviceAuthService.assertValidMqtt(env.apiKey(), env.timestamp(), env.nonce(), env.signature(),
                    env.data().getBytes(StandardCharsets.UTF_8));

            if (topic.endsWith("/device-reading")) {
                DeviceReadingIngestRequest req = JSONUtil.toBean(env.data(), DeviceReadingIngestRequest.class);
                ingestService.ingestDeviceReading(req.deviceCode(), req.realValue(), req.systime());
                String companyCode = ingestService.tryResolveCompanyCodeByDeviceOrCamera(req.deviceCode());
                hardwareIngestLogService.record("MQTT", topic, "device-reading", env.apiKey(), companyCode, payload, true, null);
                return;
            }
            hardwareIngestLogService.record("MQTT", topic, messageType, env.apiKey(), null, payload, false, "不支持的 MQTT topic（仅 device-reading 走 MQTT）");
        } catch (Exception e) {
            hardwareIngestLogService.record("MQTT", topic, messageType, apiKey, null, payload, false, e.getMessage());
            log.warn("MQTT 接入处理失败 topic={} payload={} err={}", topic, safe(payload), e.getMessage());
        }
    }

    private static String safe(String payload) {
        if (payload == null) return "";
        if (payload.length() <= 512) return payload;
        return payload.substring(0, 512) + "...";
    }

    private String resolveMessageType(String topic) {
        if (topic == null) return "unknown";
        if (topic.endsWith("/device-reading")) return "device-reading";
        if (topic.endsWith("/alarm")) return "alarm";
        if (topic.endsWith("/person-inout")) return "person-inout";
        if (topic.endsWith("/car-inout")) return "car-inout";
        if (topic.endsWith("/server-heartbeat")) return "server-heartbeat";
        return "unknown";
    }

    @Override
    public void deliveryComplete(IMqttDeliveryToken token) {
        // consumer 不用
    }

    @Override
    public void destroy() {
        MqttClient c = this.client;
        if (c == null) return;
        try {
            if (c.isConnected()) c.disconnect();
        } catch (Exception ignored) {
        }
        try {
            c.close();
        } catch (Exception ignored) {
        }
    }
}
