package com.safetyfire.monitor.service;

import com.safetyfire.monitor.common.BizException;
import com.safetyfire.monitor.common.ErrorCode;
import com.safetyfire.monitor.common.PageResponse;
import com.safetyfire.monitor.domain.dto.CompanyCreateRequest;
import com.safetyfire.monitor.domain.dto.CompanyUpdateRequest;
import com.safetyfire.monitor.domain.entity.CompanyEntity;
import com.safetyfire.monitor.domain.vo.CompanyVO;
import com.safetyfire.monitor.mapper.CompanyMapper;
import com.safetyfire.monitor.security.DataScopeService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

/**
 * 企业服务：企业档案维护（FR-01）。
 */
@Service
public class CompanyService {
    private final CompanyMapper companyMapper;
    private final DataScopeService dataScopeService;

    public CompanyService(CompanyMapper companyMapper, DataScopeService dataScopeService) {
        this.companyMapper = companyMapper;
        this.dataScopeService = dataScopeService;
    }

    @Transactional
    public Long create(CompanyCreateRequest req) {
        // 数据权限：创建企业本身属于“写企业”，此处仍按 companyCode 授权校验（非 ADMIN 必须被授权到该企业编码）
        dataScopeService.assertCompanyAllowed(req.companyCode());
        if (companyMapper.findByCompanyCode(req.companyCode()) != null) {
            throw new BizException(ErrorCode.STATE_CONFLICT, "企业编码已存在");
        }
        CompanyEntity e = new CompanyEntity();
        e.setCompanyCode(req.companyCode());
        e.setCompanyName(req.companyName());
        e.setBusinessLicense(req.businessLicense());
        e.setBusinessLicenseFileId(req.businessLicenseFileId());
        e.setBusinessLicenseStart(req.businessLicenseStart());
        e.setBusinessLicenseEnd(req.businessLicenseEnd());
        e.setBusinessLicenseScope(req.businessLicenseScope());
        e.setBusinessLicenseIssuingAuthority(req.businessLicenseIssuingAuthority());
        e.setAddress(req.address());
        e.setRegisterAddress(req.registerAddress());
        e.setCompanyStatus(req.companyStatus());
        e.setDosage(req.dosage());
        e.setReservoirArea(req.reservoirArea());
        companyMapper.insert(e);
        return e.getId();
    }

    @Transactional
    public void update(CompanyUpdateRequest req) {
        CompanyEntity existing = companyMapper.findById(req.id());
        if (existing == null) throw new BizException(ErrorCode.NOT_FOUND, "企业不存在");
        dataScopeService.assertCompanyAllowed(existing.getCompanyCode());
        existing.setCompanyName(req.companyName());
        existing.setBusinessLicense(req.businessLicense());
        existing.setBusinessLicenseFileId(req.businessLicenseFileId());
        existing.setBusinessLicenseStart(req.businessLicenseStart());
        existing.setBusinessLicenseEnd(req.businessLicenseEnd());
        existing.setBusinessLicenseScope(req.businessLicenseScope());
        existing.setBusinessLicenseIssuingAuthority(req.businessLicenseIssuingAuthority());
        existing.setAddress(req.address());
        existing.setRegisterAddress(req.registerAddress());
        existing.setCompanyStatus(req.companyStatus());
        existing.setDosage(req.dosage());
        existing.setReservoirArea(req.reservoirArea());
        companyMapper.update(existing);
    }

    @Transactional
    public void delete(Long id) {
        CompanyEntity existing = companyMapper.findById(id);
        if (existing == null) throw new BizException(ErrorCode.NOT_FOUND, "企业不存在");
        dataScopeService.assertCompanyAllowed(existing.getCompanyCode());
        int n = companyMapper.deleteById(id);
        if (n <= 0) throw new BizException(ErrorCode.NOT_FOUND, "企业不存在");
    }

    public CompanyVO get(Long id) {
        CompanyEntity e = companyMapper.findById(id);
        if (e == null) throw new BizException(ErrorCode.NOT_FOUND, "企业不存在");
        dataScopeService.assertCompanyAllowed(e.getCompanyCode());
        return toVo(e);
    }

    public PageResponse<CompanyVO> list(String keyword, int page, int pageSize) {
        int offset = (page - 1) * pageSize;
        List<String> scope = dataScopeService.currentCompanyCodesOrAll();
        List<CompanyEntity> list = companyMapper.list(keyword, scope, offset, pageSize);
        long total = companyMapper.count(keyword, scope);
        return new PageResponse<>(list.stream().map(this::toVo).toList(), page, pageSize, total);
    }

    private CompanyVO toVo(CompanyEntity e) {
        return new CompanyVO(
                e.getId(),
                e.getCompanyCode(),
                e.getCompanyName(),
                e.getBusinessLicense(),
                e.getBusinessLicenseFileId(),
                e.getBusinessLicenseStart(),
                e.getBusinessLicenseEnd(),
                e.getBusinessLicenseScope(),
                e.getBusinessLicenseIssuingAuthority(),
                e.getAddress(),
                e.getRegisterAddress(),
                e.getCompanyStatus(),
                e.getDosage(),
                e.getReservoirArea()
        );
    }
}
