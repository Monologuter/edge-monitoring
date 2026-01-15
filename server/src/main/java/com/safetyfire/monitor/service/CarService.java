package com.safetyfire.monitor.service;

import com.safetyfire.monitor.common.BizException;
import com.safetyfire.monitor.common.ErrorCode;
import com.safetyfire.monitor.common.PageResponse;
import com.safetyfire.monitor.domain.dto.CarCreateRequest;
import com.safetyfire.monitor.domain.dto.CarUpdateRequest;
import com.safetyfire.monitor.domain.entity.CarEntity;
import com.safetyfire.monitor.domain.vo.CarVO;
import com.safetyfire.monitor.domain.vo.ImportResultVO;
import com.safetyfire.monitor.mapper.CarMapper;
import com.safetyfire.monitor.security.DataScopeService;
import com.safetyfire.monitor.util.CsvImportUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import java.time.ZoneId;
import java.util.List;
import java.util.Map;

/**
 * 车辆服务：档案维护（FR-03）。
 */
@Service
public class CarService {
    private final CarMapper carMapper;
    private final DataScopeService dataScopeService;

    public CarService(CarMapper carMapper, DataScopeService dataScopeService) {
        this.carMapper = carMapper;
        this.dataScopeService = dataScopeService;
    }

    @Transactional
    public Long create(CarCreateRequest req) {
        dataScopeService.assertCompanyAllowed(req.companyCode());
        if (carMapper.findByPlate(req.licensePlateNumber()) != null) {
            throw new BizException(ErrorCode.STATE_CONFLICT, "车牌号已存在");
        }
        CarEntity e = new CarEntity();
        e.setCompanyCode(req.companyCode());
        e.setLicensePlateNumber(req.licensePlateNumber());
        e.setDriverName(req.driverName());
        e.setDriverPhone(req.driverPhone());
        e.setValidStart(req.validStart());
        e.setValidEnd(req.validEnd());
        e.setLicenseFileId(req.licenseFileId());
        e.setCarType(req.carType());
        carMapper.insert(e);
        return e.getId();
    }

    @Transactional
    public void update(CarUpdateRequest req) {
        CarEntity e = carMapper.findById(req.id());
        if (e == null) throw new BizException(ErrorCode.NOT_FOUND, "车辆不存在");
        dataScopeService.assertCompanyAllowed(e.getCompanyCode());
        dataScopeService.assertCompanyAllowed(req.companyCode());
        CarEntity byPlate = carMapper.findByPlate(req.licensePlateNumber());
        if (byPlate != null && !byPlate.getId().equals(req.id())) {
            throw new BizException(ErrorCode.STATE_CONFLICT, "车牌号已存在");
        }
        e.setCompanyCode(req.companyCode());
        e.setLicensePlateNumber(req.licensePlateNumber());
        e.setDriverName(req.driverName());
        e.setDriverPhone(req.driverPhone());
        e.setValidStart(req.validStart());
        e.setValidEnd(req.validEnd());
        e.setLicenseFileId(req.licenseFileId());
        e.setCarType(req.carType());
        carMapper.update(e);
    }

    @Transactional
    public void delete(Long id) {
        CarEntity e = carMapper.findById(id);
        if (e == null) throw new BizException(ErrorCode.NOT_FOUND, "车辆不存在");
        dataScopeService.assertCompanyAllowed(e.getCompanyCode());
        int n = carMapper.deleteById(id);
        if (n <= 0) throw new BizException(ErrorCode.NOT_FOUND, "车辆不存在");
    }

    public PageResponse<CarVO> list(String keyword, int page, int pageSize) {
        int offset = (page - 1) * pageSize;
        List<String> scope = dataScopeService.currentCompanyCodesOrAll();
        List<CarEntity> list = carMapper.list(keyword, scope, offset, pageSize);
        long total = carMapper.count(keyword, scope);
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
                String plate = CsvImportUtils.firstNonBlank(row, "licensePlateNumber", "车牌号");
                if (companyCode.isBlank() || plate.isBlank()) {
                    throw new BizException(ErrorCode.PARAM_INVALID, "企业编码/车牌号不能为空");
                }
                dataScopeService.assertCompanyAllowed(companyCode);
                String driverName = CsvImportUtils.firstNonBlank(row, "driverName", "司机姓名");
                String driverPhone = CsvImportUtils.firstNonBlank(row, "driverPhone", "司机手机号");
                java.time.LocalDate validStart = parseDate(CsvImportUtils.firstNonBlank(row, "validStart", "有效开始时间"));
                java.time.LocalDate validEnd = parseDate(CsvImportUtils.firstNonBlank(row, "validEnd", "有效结束时间"));
                String carType = CsvImportUtils.firstNonBlank(row, "carType", "车辆类型");
                Long licenseFileId = parseLong(CsvImportUtils.firstNonBlank(row, "licenseFileId", "行驶证上传"));

                CarEntity existing = carMapper.findByPlate(plate);
                if (existing == null) {
                    CarEntity e = new CarEntity();
                    e.setCompanyCode(companyCode);
                    e.setLicensePlateNumber(plate);
                    e.setDriverName(blankToNull(driverName));
                    e.setDriverPhone(blankToNull(driverPhone));
                    e.setValidStart(validStart);
                    e.setValidEnd(validEnd);
                    e.setLicenseFileId(licenseFileId);
                    e.setCarType(blankToNull(carType));
                    carMapper.insert(e);
                } else {
                    dataScopeService.assertCompanyAllowed(existing.getCompanyCode());
                    existing.setCompanyCode(companyCode);
                    existing.setDriverName(blankToNull(driverName));
                    existing.setDriverPhone(blankToNull(driverPhone));
                    existing.setValidStart(validStart);
                    existing.setValidEnd(validEnd);
                    existing.setLicenseFileId(licenseFileId);
                    existing.setCarType(blankToNull(carType));
                    carMapper.update(existing);
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

    private Long parseLong(String value) {
        if (value == null || value.isBlank()) return null;
        try {
            return Long.valueOf(value.trim());
        } catch (Exception e) {
            return null;
        }
    }

    private CarVO toVo(CarEntity e) {
        return new CarVO(
                e.getId(),
                e.getCompanyCode(),
                e.getLicensePlateNumber(),
                e.getDriverName(),
                e.getDriverPhone(),
                e.getValidStart(),
                e.getValidEnd(),
                e.getLicenseFileId(),
                e.getCarType(),
                e.getUpdatedAt() == null ? null : e.getUpdatedAt().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli()
        );
    }
}
