package com.safetyfire.monitor.service;

import com.safetyfire.monitor.domain.dto.ServerHeartbeatIngestRequest;
import com.safetyfire.monitor.domain.entity.ServerHeartbeatEntity;
import com.safetyfire.monitor.domain.entity.ServerMachineEntity;
import com.safetyfire.monitor.mapper.ServerHeartbeatMapper;
import com.safetyfire.monitor.mapper.ServerMachineMapper;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * 服务器心跳服务：维护服务器信息与心跳入库。
 */
@Service
public class ServerHeartbeatService {
    private final ServerMachineMapper serverMachineMapper;
    private final ServerHeartbeatMapper serverHeartbeatMapper;

    public ServerHeartbeatService(ServerMachineMapper serverMachineMapper, ServerHeartbeatMapper serverHeartbeatMapper) {
        this.serverMachineMapper = serverMachineMapper;
        this.serverHeartbeatMapper = serverHeartbeatMapper;
    }

    @Transactional
    public void ingest(ServerHeartbeatIngestRequest req) {
        ServerMachineEntity exists = serverMachineMapper.findByIp(req.ip());
        if (exists == null) {
            ServerMachineEntity e = new ServerMachineEntity();
            e.setCompanyCode(req.companyCode());
            e.setComputerName(req.computerName());
            e.setIp(req.ip());
            e.setOriginalId(req.originalId());
            serverMachineMapper.insert(e);
        } else {
            exists.setCompanyCode(req.companyCode());
            exists.setComputerName(req.computerName());
            exists.setOriginalId(req.originalId());
            serverMachineMapper.update(exists);
        }

        ServerHeartbeatEntity hb = new ServerHeartbeatEntity();
        hb.setIp(req.ip());
        hb.setHeartbeatTime(req.heartbeatTime());
        serverHeartbeatMapper.insert(hb);
    }
}

