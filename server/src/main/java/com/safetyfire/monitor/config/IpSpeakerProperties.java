package com.safetyfire.monitor.config;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

/**
 * IP 音柱 HTTP 接口配置。
 */
@Component
@ConfigurationProperties(prefix = "app.ip-speaker")
public class IpSpeakerProperties {
    /**
     * 音柱 HTTP 接口地址（完整 URL）。
     */
    private String apiUrl;

    /**
     * 是否启用音柱联动。
     */
    private boolean enabled;

    /**
     * 音量（0-100）。
     */
    private int volume;

    /**
     * 播放次数。
     */
    private int playCount;

    /**
     * 播放间隔（秒）。
     */
    private int playInterval;

    /**
     * 是否进入队列播放。
     */
    private boolean queue;

    /**
     * 是否同步返回（音柱要求）。
     */
    private boolean syncResponse;

    /**
     * TTS 音调。
     */
    private int ttsPitch;

    /**
     * TTS 语速。
     */
    private int ttsSpeed;

    /**
     * TTS 发音人名称。
     */
    private String ttsVoiceName;

    /**
     * 连接超时（毫秒）。
     */
    private int connectTimeoutMs;

    /**
     * 读取超时（毫秒）。
     */
    private int readTimeoutMs;

    public String getApiUrl() {
        return apiUrl;
    }

    public void setApiUrl(String apiUrl) {
        this.apiUrl = apiUrl;
    }

    public boolean isEnabled() {
        return enabled;
    }

    public void setEnabled(boolean enabled) {
        this.enabled = enabled;
    }

    public int getVolume() {
        return volume;
    }

    public void setVolume(int volume) {
        this.volume = volume;
    }

    public int getPlayCount() {
        return playCount;
    }

    public void setPlayCount(int playCount) {
        this.playCount = playCount;
    }

    public int getPlayInterval() {
        return playInterval;
    }

    public void setPlayInterval(int playInterval) {
        this.playInterval = playInterval;
    }

    public boolean isQueue() {
        return queue;
    }

    public void setQueue(boolean queue) {
        this.queue = queue;
    }

    public boolean isSyncResponse() {
        return syncResponse;
    }

    public void setSyncResponse(boolean syncResponse) {
        this.syncResponse = syncResponse;
    }

    public int getTtsPitch() {
        return ttsPitch;
    }

    public void setTtsPitch(int ttsPitch) {
        this.ttsPitch = ttsPitch;
    }

    public int getTtsSpeed() {
        return ttsSpeed;
    }

    public void setTtsSpeed(int ttsSpeed) {
        this.ttsSpeed = ttsSpeed;
    }

    public String getTtsVoiceName() {
        return ttsVoiceName;
    }

    public void setTtsVoiceName(String ttsVoiceName) {
        this.ttsVoiceName = ttsVoiceName;
    }

    public int getConnectTimeoutMs() {
        return connectTimeoutMs;
    }

    public void setConnectTimeoutMs(int connectTimeoutMs) {
        this.connectTimeoutMs = connectTimeoutMs;
    }

    public int getReadTimeoutMs() {
        return readTimeoutMs;
    }

    public void setReadTimeoutMs(int readTimeoutMs) {
        this.readTimeoutMs = readTimeoutMs;
    }
}
