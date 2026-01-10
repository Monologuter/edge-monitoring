package com.safetyfire.monitor.service;

import com.safetyfire.monitor.mapper.DeviceMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

/**
 * 设备在线状态维护：根据心跳超时将设备标记离线。
 */
@EnableScheduling
@Component
public class DeviceOnlineScheduler {
    private static final Logger log = LoggerFactory.getLogger(DeviceOnlineScheduler.class);

    private final DeviceMapper deviceMapper;

    public DeviceOnlineScheduler(DeviceMapper deviceMapper) {
        this.deviceMapper = deviceMapper;
    }

    @Scheduled(fixedDelay = 30_000)
    public void markOffline() {
        // 超过 2 分钟无心跳则离线（可配置化，先按默认）
        long threshold = System.currentTimeMillis() - 120_000L;
        int changed = deviceMapper.markOfflineBefore(threshold);
        if (changed > 0) {
            log.info("设备离线状态更新 count={}", changed);
        }
    }
}

