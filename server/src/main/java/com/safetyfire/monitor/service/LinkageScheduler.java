package com.safetyfire.monitor.service;

import com.safetyfire.monitor.domain.entity.LinkageEventEntity;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

/**
 * 联动调度：轮询执行待发送事件。
 */
@Component
public class LinkageScheduler {
    private static final Logger log = LoggerFactory.getLogger(LinkageScheduler.class);
    private final LinkageService linkageService;

    public LinkageScheduler(LinkageService linkageService) {
        this.linkageService = linkageService;
    }

    @Scheduled(fixedDelay = 2000)
    public void run() {
        for (LinkageEventEntity e : linkageService.pickPending(20)) {
            try {
                linkageService.execute(e);
                linkageService.markSent(e.getId());
            } catch (Exception ex) {
                linkageService.markFailed(e.getId(), ex.getMessage());
                log.warn("联动执行失败 id={} type={} target={} err={}", e.getId(), e.getLinkageType(), e.getTarget(), ex.getMessage());
            }
        }
    }
}

