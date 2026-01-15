package com.safetyfire.monitor.domain.vo;

/**
 * 服务器信息视图对象。
 */
public record ServerMachineVO(
        Long id,
        String companyCode,
        String computerName,
        String ip,
        String originalId,
        Integer onlineStatus,
        Long lastHeartbeatTime,
        Long createdTime
) {
}
