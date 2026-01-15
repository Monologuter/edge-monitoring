package com.safetyfire.monitor.config;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * 告警语音播报配置：用于生成 IP 音柱的 TTS 文本。
 */
@Component
@ConfigurationProperties(prefix = "app.linkage.tts")
public class AlarmTtsProperties {
    /**
     * 是否启用告警播报。
     */
    private boolean enabled;

    /**
     * 统一前缀，例如“请注意，”。为空则不追加。
     */
    private String defaultPrefix;

    /**
     * 告警类型 -> 播报文案映射列表（避免 Map 绑定异常）。
     */
    private List<MappingItem> mapping = new ArrayList<>();

    public boolean isEnabled() {
        return enabled;
    }

    public void setEnabled(boolean enabled) {
        this.enabled = enabled;
    }

    public String getDefaultPrefix() {
        return defaultPrefix;
    }

    public void setDefaultPrefix(String defaultPrefix) {
        this.defaultPrefix = defaultPrefix;
    }

    public List<MappingItem> getMapping() {
        return mapping;
    }

    public void setMapping(List<MappingItem> mapping) {
        this.mapping = mapping;
    }

    public Map<String, String> mappingAsMap() {
        Map<String, String> out = new LinkedHashMap<>();
        if (mapping == null) return out;
        for (MappingItem item : mapping) {
            if (item == null || item.key == null || item.key.isBlank() || item.text == null) {
                continue;
            }
            out.put(item.key, item.text);
        }
        return out;
    }

    public static class MappingItem {
        private String key;
        private String text;

        public String getKey() {
            return key;
        }

        public void setKey(String key) {
            this.key = key;
        }

        public String getText() {
            return text;
        }

        public void setText(String text) {
            this.text = text;
        }
    }
}
