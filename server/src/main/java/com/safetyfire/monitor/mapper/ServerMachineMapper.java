package com.safetyfire.monitor.mapper;

import com.safetyfire.monitor.domain.entity.ServerMachineEntity;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * 企业服务器信息访问层。
 */
@Mapper
public interface ServerMachineMapper {
    ServerMachineEntity findByIp(@Param("ip") String ip);

    void insert(ServerMachineEntity e);

    int update(ServerMachineEntity e);

    List<ServerMachineEntity> list(@Param("companyCodes") List<String> companyCodes, @Param("offset") int offset, @Param("pageSize") int pageSize);

    long count(@Param("companyCodes") List<String> companyCodes);
}
