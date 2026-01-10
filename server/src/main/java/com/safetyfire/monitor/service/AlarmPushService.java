package com.safetyfire.monitor.service;

import com.safetyfire.monitor.domain.vo.AlarmVO;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Service;

/**
 * 告警实时推送服务。
 */
@Service
public class AlarmPushService {
    private final SimpMessagingTemplate template;

    public AlarmPushService(SimpMessagingTemplate template) {
        this.template = template;
    }

    public void push(AlarmVO vo) {
        template.convertAndSend("/topic/alarms", vo);
    }
}

