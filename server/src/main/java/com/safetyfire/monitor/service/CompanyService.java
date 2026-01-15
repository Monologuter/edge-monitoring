package com.safetyfire.monitor.service;

import com.safetyfire.monitor.common.BizException;
import com.safetyfire.monitor.common.ErrorCode;
import com.safetyfire.monitor.common.PageResponse;
import com.safetyfire.monitor.domain.dto.CompanyCreateRequest;
import com.safetyfire.monitor.domain.dto.CompanyUpdateRequest;
import com.safetyfire.monitor.domain.entity.CompanyEntity;
import com.safetyfire.monitor.domain.vo.CompanyVO;
import com.safetyfire.monitor.domain.vo.ImportResultVO;
import com.safetyfire.monitor.mapper.CompanyMapper;
import com.safetyfire.monitor.security.DataScopeService;
import com.safetyfire.monitor.util.CsvImportUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;
import java.util.Map;

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
        e.setCreditCode(req.creditCode());
        e.setPrincipalName(req.principalName());
        e.setBusinessLicense(req.businessLicense());
        e.setBusinessLicenseFileId(req.businessLicenseFileId());
        e.setBusinessLicenseStart(req.businessLicenseStart());
        e.setBusinessLicenseEnd(req.businessLicenseEnd());
        e.setBusinessLicenseScope(req.businessLicenseScope());
        e.setBusinessLicenseIssuingAuthority(req.businessLicenseIssuingAuthority());
        e.setAddress(req.address());
        e.setRegisterAddress(req.registerAddress());
        e.setStorageAddress(req.storageAddress());
        e.setCompanyStatus(req.companyStatus());
        e.setDosage(req.dosage());
        e.setReservoirArea(req.reservoirArea());
        e.setStoreroomArea(req.storeroomArea());
        e.setLongitude(req.longitude());
        e.setLatitude(req.latitude());
        companyMapper.insert(e);
        return e.getId();
    }

    @Transactional
    public void update(CompanyUpdateRequest req) {
        CompanyEntity existing = companyMapper.findById(req.id());
        if (existing == null) throw new BizException(ErrorCode.NOT_FOUND, "企业不存在");
        dataScopeService.assertCompanyAllowed(existing.getCompanyCode());
        existing.setCompanyName(req.companyName());
        existing.setCreditCode(req.creditCode());
        existing.setPrincipalName(req.principalName());
        existing.setBusinessLicense(req.businessLicense());
        existing.setBusinessLicenseFileId(req.businessLicenseFileId());
        existing.setBusinessLicenseStart(req.businessLicenseStart());
        existing.setBusinessLicenseEnd(req.businessLicenseEnd());
        existing.setBusinessLicenseScope(req.businessLicenseScope());
        existing.setBusinessLicenseIssuingAuthority(req.businessLicenseIssuingAuthority());
        existing.setAddress(req.address());
        existing.setRegisterAddress(req.registerAddress());
        existing.setStorageAddress(req.storageAddress());
        existing.setCompanyStatus(req.companyStatus());
        existing.setDosage(req.dosage());
        existing.setReservoirArea(req.reservoirArea());
        existing.setStoreroomArea(req.storeroomArea());
        existing.setLongitude(req.longitude());
        existing.setLatitude(req.latitude());
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

    @Transactional
    public ImportResultVO importCsv(MultipartFile file) {
        if (file == null || file.isEmpty()) {
            throw new BizException(ErrorCode.PARAM_INVALID, "导入文件为空");
        }
        int success = 0;
        int fail = 0;
        List<String> errors = new java.util.ArrayList<>();
        List<Map<String, String>> rows = CsvImportUtils.parse(getInputStream(file));
        for (int i = 0; i < rows.size(); i++) {
            Map<String, String> row = rows.get(i);
            try {
                String companyCode = CsvImportUtils.firstNonBlank(row, "companyCode", "企业流向系统代码", "企业编码");
                String companyName = CsvImportUtils.firstNonBlank(row, "companyName", "企业名称");
                if (companyCode.isBlank() || companyName.isBlank()) {
                    throw new BizException(ErrorCode.PARAM_INVALID, "企业编码/名称不能为空");
                }
                String creditCode = CsvImportUtils.firstNonBlank(row, "creditCode", "信用代码");
                String principalName = CsvImportUtils.firstNonBlank(row, "principalName", "主要负责人");
                String businessLicense = CsvImportUtils.firstNonBlank(row, "businessLicense", "许可证编号");
                java.time.LocalDate businessLicenseStart = parseDate(CsvImportUtils.firstNonBlank(row, "businessLicenseStart", "许可证开始日期"));
                java.time.LocalDate businessLicenseEnd = parseDate(CsvImportUtils.firstNonBlank(row, "businessLicenseEnd", "许可证截止日期"));
                String issuing = CsvImportUtils.firstNonBlank(row, "businessLicenseIssuingAuthority", "许可证发证机关");
                String registerAddress = CsvImportUtils.firstNonBlank(row, "registerAddress", "注册地址");
                String storageAddress = CsvImportUtils.firstNonBlank(row, "storageAddress", "仓储地址");
                String address = CsvImportUtils.firstNonBlank(row, "address", "企业地址");
                Double storeroomArea = parseDouble(CsvImportUtils.firstNonBlank(row, "storeroomArea", "库房面积（m²）", "库房面积"));
                Double reservoirArea = parseDouble(CsvImportUtils.firstNonBlank(row, "reservoirArea", "库区面积（m²）", "库区面积"));
                Double dosage = parseDouble(CsvImportUtils.firstNonBlank(row, "dosage", "核定药量（kg）", "核定药量"));
                Double longitude = parseDouble(CsvImportUtils.firstNonBlank(row, "longitude", "经度"));
                Double latitude = parseDouble(CsvImportUtils.firstNonBlank(row, "latitude", "纬度"));

                CompanyEntity existing = companyMapper.findByCompanyCode(companyCode);
                if (existing == null) {
                    dataScopeService.assertCompanyAllowed(companyCode);
                    CompanyEntity e = new CompanyEntity();
                    e.setCompanyCode(companyCode);
                    e.setCompanyName(companyName);
                    e.setCreditCode(blankToNull(creditCode));
                    e.setPrincipalName(blankToNull(principalName));
                    e.setBusinessLicense(blankToNull(businessLicense));
                    e.setBusinessLicenseStart(businessLicenseStart);
                    e.setBusinessLicenseEnd(businessLicenseEnd);
                    e.setBusinessLicenseIssuingAuthority(blankToNull(issuing));
                    e.setRegisterAddress(blankToNull(registerAddress));
                    e.setStorageAddress(blankToNull(storageAddress));
                    e.setAddress(blankToNull(address));
                    e.setStoreroomArea(storeroomArea);
                    e.setReservoirArea(reservoirArea);
                    e.setDosage(dosage);
                    e.setLongitude(longitude);
                    e.setLatitude(latitude);
                    companyMapper.insert(e);
                } else {
                    dataScopeService.assertCompanyAllowed(existing.getCompanyCode());
                    existing.setCompanyName(companyName);
                    existing.setCreditCode(blankToNull(creditCode));
                    existing.setPrincipalName(blankToNull(principalName));
                    existing.setBusinessLicense(blankToNull(businessLicense));
                    existing.setBusinessLicenseStart(businessLicenseStart);
                    existing.setBusinessLicenseEnd(businessLicenseEnd);
                    existing.setBusinessLicenseIssuingAuthority(blankToNull(issuing));
                    existing.setRegisterAddress(blankToNull(registerAddress));
                    existing.setStorageAddress(blankToNull(storageAddress));
                    existing.setAddress(blankToNull(address));
                    existing.setStoreroomArea(storeroomArea);
                    existing.setReservoirArea(reservoirArea);
                    existing.setDosage(dosage);
                    existing.setLongitude(longitude);
                    existing.setLatitude(latitude);
                    companyMapper.update(existing);
                }
                success++;
            } catch (Exception e) {
                fail++;
                errors.add("第" + (i + 2) + "行: " + e.getMessage());
            }
        }
        return new ImportResultVO(success, fail, errors);
    }

    private java.io.InputStream getInputStream(MultipartFile file) {
        try {
            return file.getInputStream();
        } catch (Exception e) {
            throw new BizException(ErrorCode.PARAM_INVALID, "读取导入文件失败");
        }
    }

    private Double parseDouble(String value) {
        if (value == null || value.isBlank()) return null;
        try {
            return Double.valueOf(value.trim());
        } catch (Exception e) {
            return null;
        }
    }

    private java.time.LocalDate parseDate(String value) {
        if (value == null || value.isBlank()) return null;
        try {
            return java.time.LocalDate.parse(value.trim());
        } catch (Exception e) {
            return null;
        }
    }

    private String blankToNull(String value) {
        return value == null || value.isBlank() ? null : value.trim();
    }

    private CompanyVO toVo(CompanyEntity e) {
        return new CompanyVO(
                e.getId(),
                e.getCompanyCode(),
                e.getCompanyName(),
                e.getCreditCode(),
                e.getPrincipalName(),
                e.getBusinessLicense(),
                e.getBusinessLicenseFileId(),
                e.getBusinessLicenseStart(),
                e.getBusinessLicenseEnd(),
                e.getBusinessLicenseScope(),
                e.getBusinessLicenseIssuingAuthority(),
                e.getAddress(),
                e.getRegisterAddress(),
                e.getStorageAddress(),
                e.getCompanyStatus(),
                e.getDosage(),
                e.getReservoirArea(),
                e.getStoreroomArea(),
                e.getLongitude(),
                e.getLatitude()
        );
    }
}
