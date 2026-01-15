package com.safetyfire.monitor.service;

import com.safetyfire.monitor.common.BizException;
import com.safetyfire.monitor.common.ErrorCode;
import com.safetyfire.monitor.common.PageResponse;
import com.safetyfire.monitor.domain.entity.AlarmEntity;
import com.safetyfire.monitor.domain.entity.AlarmActionLogEntity;
import com.safetyfire.monitor.domain.vo.AlarmVO;
import com.safetyfire.monitor.mapper.AlarmActionLogMapper;
import com.safetyfire.monitor.mapper.AlarmMapper;
import com.safetyfire.monitor.security.AuthUser;
import com.safetyfire.monitor.security.AuthUserHolder;
import com.safetyfire.monitor.security.DataScopeService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

/**
 * 告警服务：查询、处理、消警。
 */
@Service
public class AlarmService {
    private final AlarmMapper alarmMapper;
    private final AlarmActionLogMapper alarmActionLogMapper;
    private final AlarmPushService alarmPushService;
    private final DataScopeService dataScopeService;
    private final AlarmTtsService alarmTtsService;

    public AlarmService(AlarmMapper alarmMapper, AlarmActionLogMapper alarmActionLogMapper, AlarmPushService alarmPushService,
                        DataScopeService dataScopeService, AlarmTtsService alarmTtsService) {
        this.alarmMapper = alarmMapper;
        this.alarmActionLogMapper = alarmActionLogMapper;
        this.alarmPushService = alarmPushService;
        this.dataScopeService = dataScopeService;
        this.alarmTtsService = alarmTtsService;
    }

    public PageResponse<AlarmVO> list(int page, int pageSize, String status) {
        int offset = (page - 1) * pageSize;
        List<String> scope = dataScopeService.currentCompanyCodesOrAll();
        List<AlarmEntity> list = alarmMapper.list(scope, offset, pageSize, status);
        long total = alarmMapper.count(scope, status);
        return new PageResponse<>(list.stream().map(this::toVo).toList(), page, pageSize, total);
    }

    @Transactional
    public void handle(Long alarmId, String action, String remark) {
        AlarmEntity alarm = alarmMapper.findById(alarmId);
        if (alarm == null) {
            throw new BizException(ErrorCode.NOT_FOUND, "告警不存在");
        }
        if (alarm.getCompanyCode() != null) {
            dataScopeService.assertCompanyAllowed(alarm.getCompanyCode());
        } else if (dataScopeService.currentCompanyCodesOrAll() != null) {
            throw new BizException(ErrorCode.FORBIDDEN, "无该告警数据权限");
        }
        AuthUser user = AuthUserHolder.get();
        String handler = user == null ? "system" : user.username();

        String normalized = action == null ? "" : action.trim().toUpperCase();
        if ("HANDLE".equals(normalized)) {
            alarmMapper.updateHandle(alarmId, handler, remark);
            insertAction(alarmId, "HANDLE", handler, remark);
            alarmPushService.push(toVo(alarmMapper.findById(alarmId)));
            return;
        }
        if ("CLEAR".equals(normalized)) {
            alarmMapper.updateClear(alarmId, System.currentTimeMillis(), handler, remark);
            insertAction(alarmId, "CLEAR", handler, remark);
            alarmPushService.push(toVo(alarmMapper.findById(alarmId)));
            return;
        }
        if ("ARCHIVE".equals(normalized)) {
            alarmMapper.updateArchive(alarmId, System.currentTimeMillis(), handler, remark);
            insertAction(alarmId, "ARCHIVE", handler, remark);
            alarmPushService.push(toVo(alarmMapper.findById(alarmId)));
            return;
        }
        throw new BizException(ErrorCode.PARAM_INVALID, "action 仅支持 HANDLE/CLEAR/ARCHIVE");
    }

    public void created(Long alarmId, String operator, String remark) {
        insertAction(alarmId, "CREATE", operator, remark);
        AlarmEntity alarm = alarmMapper.findById(alarmId);
        if (alarm != null) {
            alarmPushService.push(toVo(alarm));
            alarmTtsService.onAlarmCreated(alarm);
        }
    }

    private void insertAction(Long alarmId, String action, String operator, String remark) {
        AlarmActionLogEntity log = new AlarmActionLogEntity();
        log.setAlarmId(alarmId);
        log.setAction(action);
        log.setOperator(operator);
        log.setRemark(remark);
        log.setActionTime(System.currentTimeMillis());
        alarmActionLogMapper.insert(log);
    }

    private AlarmVO toVo(AlarmEntity e) {
        return new AlarmVO(
                e.getId(),
                e.getCompanyCode(),
                e.getAlarmType(),
                e.getAlarmStatus(),
                e.getWorkflowStatus(),
                e.getRiskLevel(),
                e.getDeviceCode(),
                e.getAlarmFile(),
                e.getStoreNum(),
                e.getStoreroomNum(),
                e.getWarningTime(),
                e.getClearTime(),
                e.getArchivedTime(),
                e.getHandledTime(),
                e.getHandler(),
                e.getRemark()
        );
    }
}
