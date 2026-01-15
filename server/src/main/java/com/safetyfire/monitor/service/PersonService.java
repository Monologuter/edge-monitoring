package com.safetyfire.monitor.service;

import com.safetyfire.monitor.common.BizException;
import com.safetyfire.monitor.common.ErrorCode;
import com.safetyfire.monitor.common.PageResponse;
import com.safetyfire.monitor.domain.dto.PersonCreateRequest;
import com.safetyfire.monitor.domain.dto.PersonUpdateRequest;
import com.safetyfire.monitor.domain.entity.PersonEntity;
import com.safetyfire.monitor.domain.vo.PersonVO;
import com.safetyfire.monitor.domain.vo.ImportResultVO;
import com.safetyfire.monitor.mapper.PersonMapper;
import com.safetyfire.monitor.security.DataScopeService;
import com.safetyfire.monitor.util.MaskUtils;
import com.safetyfire.monitor.util.CsvImportUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import java.time.ZoneId;
import java.util.List;
import java.util.Map;

/**
 * 人员服务：档案维护（FR-02）。
 */
@Service
public class PersonService {
    private final PersonMapper personMapper;
    private final DataScopeService dataScopeService;
    private final IngestService ingestService;

    public PersonService(PersonMapper personMapper, DataScopeService dataScopeService, IngestService ingestService) {
        this.personMapper = personMapper;
        this.dataScopeService = dataScopeService;
        this.ingestService = ingestService;
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
        e.setAvatarFileId(req.avatarFileId());
        e.setCertFileId(req.certFileId());
        e.setCertExpireDate(req.certExpireDate());
        e.setSmsNotify(req.smsNotify() == null ? 0 : req.smsNotify());
        personMapper.insert(e);
        maybeAlarmOnCertificate(e);
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
        e.setAvatarFileId(req.avatarFileId());
        e.setCertFileId(req.certFileId());
        e.setCertExpireDate(req.certExpireDate());
        e.setSmsNotify(req.smsNotify() == null ? 0 : req.smsNotify());
        personMapper.update(e);
        maybeAlarmOnCertificate(e);
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
                String companyCode = CsvImportUtils.firstNonBlank(row, "companyCode", "企业code", "企业编码");
                String personName = CsvImportUtils.firstNonBlank(row, "personName", "人员姓名");
                String idcard = CsvImportUtils.firstNonBlank(row, "idcard", "身份证");
                String personType = CsvImportUtils.firstNonBlank(row, "personType", "人员类型");
                if (companyCode.isBlank() || personName.isBlank() || idcard.isBlank() || personType.isBlank()) {
                    throw new BizException(ErrorCode.PARAM_INVALID, "企业编码/姓名/身份证/人员类型不能为空");
                }
                dataScopeService.assertCompanyAllowed(companyCode);
                Integer isCertified = parseInt(CsvImportUtils.firstNonBlank(row, "isCertified", "是否有证书", "有无证书"));
                Integer smsNotify = parseInt(CsvImportUtils.firstNonBlank(row, "smsNotify", "是否短信推送"));
                java.time.LocalDate certExpireDate = parseDate(CsvImportUtils.firstNonBlank(row, "certExpireDate", "证书到期日期"));
                String phone = CsvImportUtils.firstNonBlank(row, "phone", "电话");
                Long avatarFileId = parseLong(CsvImportUtils.firstNonBlank(row, "avatarFileId", "头像照片"));
                Long certFileId = parseLong(CsvImportUtils.firstNonBlank(row, "certFileId", "证书附件"));

                PersonEntity existing = personMapper.findByIdcard(idcard);
                if (existing == null) {
                    PersonEntity e = new PersonEntity();
                    e.setCompanyCode(companyCode);
                    e.setPersonName(personName);
                    e.setIdcard(idcard);
                    e.setPersonType(personType);
                    e.setIsCertified(isCertified == null ? 0 : isCertified);
                    e.setSmsNotify(smsNotify == null ? 0 : smsNotify);
                    e.setCertExpireDate(certExpireDate);
                    e.setPhone(blankToNull(phone));
                    e.setAvatarFileId(avatarFileId);
                    e.setCertFileId(certFileId);
                    personMapper.insert(e);
                    maybeAlarmOnCertificate(e);
                } else {
                    dataScopeService.assertCompanyAllowed(existing.getCompanyCode());
                    existing.setCompanyCode(companyCode);
                    existing.setPersonName(personName);
                    existing.setPersonType(personType);
                    existing.setIsCertified(isCertified == null ? 0 : isCertified);
                    existing.setSmsNotify(smsNotify == null ? 0 : smsNotify);
                    existing.setCertExpireDate(certExpireDate);
                    existing.setPhone(blankToNull(phone));
                    existing.setAvatarFileId(avatarFileId);
                    existing.setCertFileId(certFileId);
                    personMapper.update(existing);
                    maybeAlarmOnCertificate(existing);
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

    private String blankToNull(String value) {
        return value == null || value.isBlank() ? null : value.trim();
    }

    private java.time.LocalDate parseDate(String value) {
        if (value == null || value.isBlank()) return null;
        try {
            return java.time.LocalDate.parse(value.trim());
        } catch (Exception e) {
            return null;
        }
    }

    private Integer parseInt(String value) {
        if (value == null || value.isBlank()) return null;
        try {
            return Integer.valueOf(value.trim());
        } catch (Exception e) {
            return null;
        }
    }

    private Long parseLong(String value) {
        if (value == null || value.isBlank()) return null;
        try {
            return Long.valueOf(value.trim());
        } catch (Exception e) {
            return null;
        }
    }

    private PersonVO toVo(PersonEntity e) {
        return new PersonVO(
                e.getId(),
                e.getCompanyCode(),
                e.getPersonName(),
                MaskUtils.maskIdCard(e.getIdcard()),
                e.getPersonType(),
                e.getIsCertified(),
                e.getPhone(),
                e.getAvatarFileId(),
                e.getCertFileId(),
                e.getCertExpireDate(),
                e.getSmsNotify(),
                e.getUpdatedAt() == null ? null : e.getUpdatedAt().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli()
        );
    }

    private void maybeAlarmOnCertificate(PersonEntity e) {
        if (e == null || e.getIsCertified() == null || e.getIsCertified() != 1) {
            return;
        }
        if (e.getCertExpireDate() == null) {
            return;
        }
        if (e.getCertExpireDate().isAfter(java.time.LocalDate.now())) {
            return;
        }
        String remark = "人员:" + e.getPersonName() + " 证书到期:" + e.getCertExpireDate();
        long now = System.currentTimeMillis();
        String deviceCode = "PERSON_" + (e.getId() == null ? "UNKNOWN" : e.getId());
        // 证书过期触发告警
        ingestService.ingestAlarm("证书过期报警", "ACTIVE", now, deviceCode, remark);
    }
}
