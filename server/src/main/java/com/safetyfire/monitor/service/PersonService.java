package com.safetyfire.monitor.service;

import com.safetyfire.monitor.common.BizException;
import com.safetyfire.monitor.common.ErrorCode;
import com.safetyfire.monitor.common.PageResponse;
import com.safetyfire.monitor.domain.dto.PersonCreateRequest;
import com.safetyfire.monitor.domain.dto.PersonUpdateRequest;
import com.safetyfire.monitor.domain.entity.PersonEntity;
import com.safetyfire.monitor.domain.vo.PersonVO;
import com.safetyfire.monitor.mapper.PersonMapper;
import com.safetyfire.monitor.security.DataScopeService;
import com.safetyfire.monitor.util.MaskUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

/**
 * 人员服务：档案维护（FR-02）。
 */
@Service
public class PersonService {
    private final PersonMapper personMapper;
    private final DataScopeService dataScopeService;

    public PersonService(PersonMapper personMapper, DataScopeService dataScopeService) {
        this.personMapper = personMapper;
        this.dataScopeService = dataScopeService;
    }

    @Transactional
    public Long create(PersonCreateRequest req) {
        dataScopeService.assertCompanyAllowed(req.companyCode());
        if (personMapper.findByIdcard(req.idcard()) != null) {
            throw new BizException(ErrorCode.STATE_CONFLICT, "身份证已存在");
        }
        PersonEntity e = new PersonEntity();
        e.setCompanyCode(req.companyCode());
        e.setPersonName(req.personName());
        e.setIdcard(req.idcard());
        e.setPersonType(req.personType());
        e.setIsCertified(req.isCertified());
        e.setPhone(req.phone());
        personMapper.insert(e);
        return e.getId();
    }

    @Transactional
    public void update(PersonUpdateRequest req) {
        PersonEntity e = personMapper.findById(req.id());
        if (e == null) throw new BizException(ErrorCode.NOT_FOUND, "人员不存在");
        dataScopeService.assertCompanyAllowed(e.getCompanyCode());
        dataScopeService.assertCompanyAllowed(req.companyCode());
        e.setCompanyCode(req.companyCode());
        e.setPersonName(req.personName());
        e.setPersonType(req.personType());
        e.setIsCertified(req.isCertified());
        e.setPhone(req.phone());
        personMapper.update(e);
    }

    @Transactional
    public void delete(Long id) {
        PersonEntity e = personMapper.findById(id);
        if (e == null) throw new BizException(ErrorCode.NOT_FOUND, "人员不存在");
        dataScopeService.assertCompanyAllowed(e.getCompanyCode());
        int n = personMapper.deleteById(id);
        if (n <= 0) throw new BizException(ErrorCode.NOT_FOUND, "人员不存在");
    }

    public PageResponse<PersonVO> list(String keyword, int page, int pageSize) {
        int offset = (page - 1) * pageSize;
        List<String> scope = dataScopeService.currentCompanyCodesOrAll();
        List<PersonEntity> list = personMapper.list(keyword, scope, offset, pageSize);
        long total = personMapper.count(keyword, scope);
        return new PageResponse<>(list.stream().map(this::toVo).toList(), page, pageSize, total);
    }

    private PersonVO toVo(PersonEntity e) {
        return new PersonVO(
                e.getId(),
                e.getCompanyCode(),
                e.getPersonName(),
                MaskUtils.maskIdCard(e.getIdcard()),
                e.getPersonType(),
                e.getIsCertified(),
                e.getPhone()
        );
    }
}
