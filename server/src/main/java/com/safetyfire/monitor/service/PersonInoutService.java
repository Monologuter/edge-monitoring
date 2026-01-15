package com.safetyfire.monitor.service;

import com.safetyfire.monitor.common.PageResponse;
import com.safetyfire.monitor.domain.dto.PersonInoutCreateRequest;
import com.safetyfire.monitor.domain.dto.PersonInoutIngestRequest;
import com.safetyfire.monitor.domain.dto.PersonInoutUpdateRequest;
import com.safetyfire.monitor.domain.entity.PersonInoutRecordEntity;
import com.safetyfire.monitor.domain.vo.PersonInoutRecordVO;
import com.safetyfire.monitor.mapper.PersonInoutRecordMapper;
import com.safetyfire.monitor.mapper.PersonMapper;
import com.safetyfire.monitor.security.DataScopeService;
import com.safetyfire.monitor.util.MaskUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

/**
 * 人员出入记录服务。
 */
@Service
public class PersonInoutService {
    private final PersonInoutRecordMapper mapper;
    private final PersonMapper personMapper;
    private final DataScopeService dataScopeService;
    private final IngestService ingestService;

    public PersonInoutService(PersonInoutRecordMapper mapper, PersonMapper personMapper, DataScopeService dataScopeService,
                              IngestService ingestService) {
        this.mapper = mapper;
        this.personMapper = personMapper;
        this.dataScopeService = dataScopeService;
        this.ingestService = ingestService;
    }

    @Transactional
    public String ingest(PersonInoutIngestRequest req) {
        // 尝试从人员档案反查 companyCode（用于数据权限过滤）
        String companyCode = null;
        var p = personMapper.findByIdcard(req.idcard());
        if (p != null) {
            companyCode = p.getCompanyCode();
        } else {
            // 人员不在档案中，触发非法入侵报警
            ingestService.ingestAlarm("非法入侵报警", "ACTIVE", req.inOutTime(),
                    "GATE_" + req.inOutState(), req.imageFileId() != null ? "人员未录入，ID:" + req.idcard() : "人员未录入");
        }
        PersonInoutRecordEntity e = new PersonInoutRecordEntity();
        e.setCompanyCode(companyCode);
        e.setIdcard(req.idcard());
        e.setPersonName(req.personName());
        e.setPersonType(req.personType());
        e.setInOutState(req.inOutState());
        e.setInOutTime(req.inOutTime());
        e.setImageFileId(req.imageFileId());
        mapper.insert(e);
        return companyCode;
    }

    @Transactional
    public Long create(PersonInoutCreateRequest req) {
        String companyCode = resolveCompanyCode(req.companyCode(), req.idcard());
        PersonInoutRecordEntity e = new PersonInoutRecordEntity();
        e.setCompanyCode(companyCode);
        e.setIdcard(req.idcard());
        e.setPersonName(req.personName());
        e.setPersonType(req.personType());
        e.setInOutState(req.inOutState());
        e.setInOutTime(req.inOutTime());
        e.setImageFileId(req.imageFileId());
        mapper.insert(e);
        return e.getId();
    }

    @Transactional
    public void update(PersonInoutUpdateRequest req) {
        PersonInoutRecordEntity e = mapper.findById(req.id());
        if (e == null) {
            throw new com.safetyfire.monitor.common.BizException(com.safetyfire.monitor.common.ErrorCode.NOT_FOUND, "记录不存在");
        }
        if (e.getCompanyCode() != null) {
            dataScopeService.assertCompanyAllowed(e.getCompanyCode());
        } else if (dataScopeService.currentCompanyCodesOrAll() != null) {
            throw new com.safetyfire.monitor.common.BizException(com.safetyfire.monitor.common.ErrorCode.FORBIDDEN, "无该记录数据权限");
        }
        String companyCode = resolveCompanyCode(req.companyCode(), req.idcard());
        e.setCompanyCode(companyCode);
        e.setIdcard(req.idcard());
        e.setPersonName(req.personName());
        e.setPersonType(req.personType());
        e.setInOutState(req.inOutState());
        e.setInOutTime(req.inOutTime());
        e.setImageFileId(req.imageFileId());
        mapper.update(e);
    }

    @Transactional
    public void delete(Long id) {
        PersonInoutRecordEntity e = mapper.findById(id);
        if (e == null) {
            throw new com.safetyfire.monitor.common.BizException(com.safetyfire.monitor.common.ErrorCode.NOT_FOUND, "记录不存在");
        }
        if (e.getCompanyCode() != null) {
            dataScopeService.assertCompanyAllowed(e.getCompanyCode());
        } else if (dataScopeService.currentCompanyCodesOrAll() != null) {
            throw new com.safetyfire.monitor.common.BizException(com.safetyfire.monitor.common.ErrorCode.FORBIDDEN, "无该记录数据权限");
        }
        int n = mapper.deleteById(id);
        if (n <= 0) {
            throw new com.safetyfire.monitor.common.BizException(com.safetyfire.monitor.common.ErrorCode.NOT_FOUND, "记录不存在");
        }
    }

    public PageResponse<PersonInoutRecordVO> list(int page, int pageSize) {
        int offset = (page - 1) * pageSize;
        // 数据权限：先读取全量记录再过滤会很慢；这里基于 company_code 做过滤（DB 已有字段）
        List<String> scope = dataScopeService.currentCompanyCodesOrAll();
        List<PersonInoutRecordEntity> list = mapper.listByCompanyCodes(scope, offset, pageSize);
        long total = mapper.countByCompanyCodes(scope);
        return new PageResponse<>(list.stream().map(this::toVo).toList(), page, pageSize, total);
    }

    private PersonInoutRecordVO toVo(PersonInoutRecordEntity e) {
        return new PersonInoutRecordVO(
                e.getId(),
                e.getCompanyCode(),
                e.getIdcard(),
                MaskUtils.maskIdCard(e.getIdcard()),
                e.getPersonName(),
                e.getPersonType(),
                e.getInOutState(),
                e.getInOutTime(),
                e.getImageFileId()
        );
    }

    private String resolveCompanyCode(String provided, String idcard) {
        if (provided != null && !provided.isBlank()) {
            dataScopeService.assertCompanyAllowed(provided);
            return provided.trim();
        }
        var p = personMapper.findByIdcard(idcard);
        if (p != null) {
            dataScopeService.assertCompanyAllowed(p.getCompanyCode());
            return p.getCompanyCode();
        }
        if (dataScopeService.currentCompanyCodesOrAll() != null) {
            throw new com.safetyfire.monitor.common.BizException(com.safetyfire.monitor.common.ErrorCode.PARAM_INVALID, "无法解析企业编码");
        }
        return null;
    }
}
