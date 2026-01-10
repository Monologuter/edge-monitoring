package com.safetyfire.monitor.service;

import cn.hutool.core.util.StrUtil;
import org.eclipse.paho.client.mqttv3.MqttClient;
import org.eclipse.paho.client.mqttv3.MqttConnectOptions;
import org.eclipse.paho.client.mqttv3.MqttException;
import org.eclipse.paho.client.mqttv3.MqttMessage;
import org.eclipse.paho.client.mqttv3.persist.MemoryPersistence;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.nio.charset.StandardCharsets;

/**
 * MQTT 发布器：用于音柱/TTS 的 MQTTX 协议（本质是 MQTT）。
 */
@Service
public class MqttPublisher implements DisposableBean {
    private static final Logger log = LoggerFactory.getLogger(MqttPublisher.class);

    private final boolean enabled;
    private final String brokerUrl;
    private final String username;
    private final String password;
    private final String clientIdPrefix;

    private volatile MqttClient client;

    public MqttPublisher(
            @Value("${app.mqtt.enabled}") boolean enabled,
            @Value("${app.mqtt.broker-url}") String brokerUrl,
            @Value("${app.mqtt.username}") String username,
            @Value("${app.mqtt.password}") String password,
            @Value("${app.mqtt.client-id-prefix}") String clientIdPrefix
    ) {
        this.enabled = enabled;
        this.brokerUrl = brokerUrl;
        this.username = username;
        this.password = password;
        this.clientIdPrefix = clientIdPrefix;
    }

    public void publish(String topic, String payload, int qos) throws MqttException {
        if (!enabled) {
            throw new IllegalStateException("MQTT 未启用（app.mqtt.enabled=false）");
        }
        if (StrUtil.isBlank(topic)) {
            throw new IllegalArgumentException("topic 不能为空");
        }
        if (qos < 0 || qos > 2) {
            throw new IllegalArgumentException("qos 仅支持 0/1/2");
        }

        MqttClient c = ensureConnected();
        MqttMessage msg = new MqttMessage((payload == null ? "" : payload).getBytes(StandardCharsets.UTF_8));
        msg.setQos(qos);
        msg.setRetained(false);
        c.publish(topic, msg);
    }

    private synchronized MqttClient ensureConnected() throws MqttException {
        if (client != null && client.isConnected()) {
            return client;
        }
        String clientId = clientIdPrefix + "-" + System.currentTimeMillis();
        MqttClient c = new MqttClient(brokerUrl, clientId, new MemoryPersistence());
        MqttConnectOptions opt = new MqttConnectOptions();
        opt.setAutomaticReconnect(true);
        opt.setCleanSession(true);
        opt.setConnectionTimeout(10);
        opt.setKeepAliveInterval(20);
        if (StrUtil.isNotBlank(username)) {
            opt.setUserName(username);
        }
        if (StrUtil.isNotBlank(password)) {
            opt.setPassword(password.toCharArray());
        }
        c.connect(opt);
        this.client = c;
        log.info("MQTT 已连接 brokerUrl={} clientId={}", brokerUrl, clientId);
        return c;
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

