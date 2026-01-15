package com.safetyfire.monitor.service;

import com.safetyfire.monitor.config.AlarmTtsProperties;
import com.safetyfire.monitor.domain.entity.AlarmEntity;
import com.safetyfire.monitor.domain.entity.LinkageEventEntity;
import com.safetyfire.monitor.mapper.LinkageEventMapper;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * 告警语音播报联动服务。
 */
@Service
public class AlarmTtsService {
    private final AlarmTtsProperties properties;
    private final LinkageEventMapper linkageEventMapper;

    public AlarmTtsService(AlarmTtsProperties properties, LinkageEventMapper linkageEventMapper) {
        this.properties = properties;
        this.linkageEventMapper = linkageEventMapper;
    }

    @Transactional
    public void onAlarmCreated(AlarmEntity alarm) {
        if (alarm == null || !properties.isEnabled()) {
            return;
        }
        String alarmType = alarm.getAlarmType();
        String mapped = properties.mappingAsMap().getOrDefault(alarmType, alarmType);
        if (mapped == null || mapped.isBlank()) {
            return;
        }
        String prefix = properties.getDefaultPrefix();
        String ttsText = (prefix == null || prefix.isBlank()) ? mapped : (prefix + mapped);

        LinkageEventEntity e = new LinkageEventEntity();
        e.setAlarmId(alarm.getId());
        e.setLinkageType("IP_TTS");
        e.setTarget(null);
        e.setPayload(ttsText);
        e.setStatus("PENDING");
        e.setLastError(null);
        linkageEventMapper.insert(e);
    }
}
