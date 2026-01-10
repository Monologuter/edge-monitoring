package com.safetyfire.monitor.mapper;

import com.safetyfire.monitor.domain.entity.ServerHeartbeatEntity;
import org.apache.ibatis.annotations.Mapper;

/**
 * 服务器心跳访问层。
 */
@Mapper
public interface ServerHeartbeatMapper {
    void insert(ServerHeartbeatEntity e);
}

