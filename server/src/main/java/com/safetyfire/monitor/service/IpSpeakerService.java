package com.safetyfire.monitor.service;

import com.safetyfire.monitor.config.IpSpeakerProperties;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.web.client.RestTemplateBuilder;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

import java.time.Duration;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * IP 音柱 HTTP 控制服务。
 */
@Service
public class IpSpeakerService {
    private static final Logger log = LoggerFactory.getLogger(IpSpeakerService.class);

    private final IpSpeakerProperties props;
    private final RestTemplate restTemplate;

    public IpSpeakerService(IpSpeakerProperties props, RestTemplateBuilder builder) {
        this.props = props;
        this.restTemplate = builder
                .setConnectTimeout(Duration.ofMillis(props.getConnectTimeoutMs()))
                .setReadTimeout(Duration.ofMillis(props.getReadTimeoutMs()))
                .build();
    }

    /**
     * 发送 TTS 播报指令。
     */
    public void sendTts(String ttsText, String overrideUrl) {
        if (!props.isEnabled()) {
            return;
        }
        String apiUrl = (overrideUrl == null || overrideUrl.isBlank()) ? props.getApiUrl() : overrideUrl.trim();
        if (apiUrl == null || apiUrl.isBlank()) {
            throw new IllegalArgumentException("IP 音柱 API 地址未配置");
        }

        Map<String, Object> payload = new LinkedHashMap<>();
        payload.put("command", "play_local_tts");
        payload.put("volume", props.getVolume());
        payload.put("play_count", props.getPlayCount());
        payload.put("queue", props.isQueue());
        payload.put("sync_response", props.isSyncResponse());
        payload.put("play_interval", props.getPlayInterval());
        payload.put("tts_pitch", props.getTtsPitch());
        payload.put("tts_speed", props.getTtsSpeed());
        payload.put("tts_voice_name", props.getTtsVoiceName());
        payload.put("tts_text", ttsText);

        try {
            restTemplate.postForEntity(apiUrl, payload, String.class);
        } catch (Exception e) {
            log.warn("IP 音柱指令发送失败 url={} err={}", apiUrl, e.getMessage());
            throw e;
        }
    }
}
