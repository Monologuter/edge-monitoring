package com.safetyfire.monitor.service;

import com.safetyfire.monitor.common.BizException;
import com.safetyfire.monitor.common.ErrorCode;
import com.safetyfire.monitor.common.PageResponse;
import com.safetyfire.monitor.domain.dto.StoreCreateRequest;
import com.safetyfire.monitor.domain.dto.StoreUpdateRequest;
import com.safetyfire.monitor.domain.entity.StoreEntity;
import com.safetyfire.monitor.domain.vo.StoreVO;
import com.safetyfire.monitor.domain.vo.ImportResultVO;
import com.safetyfire.monitor.mapper.StoreMapper;
import com.safetyfire.monitor.security.DataScopeService;
import com.safetyfire.monitor.util.CsvImportUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;
import java.util.Map;

/**
 * 仓库服务（FR-04）。
 */
@Service
public class StoreService {
    private final StoreMapper storeMapper;
    private final DataScopeService dataScopeService;

    public StoreService(StoreMapper storeMapper, DataScopeService dataScopeService) {
        this.storeMapper = storeMapper;
        this.dataScopeService = dataScopeService;
    }

    @Transactional
    public Long create(StoreCreateRequest req) {
        dataScopeService.assertCompanyAllowed(req.companyCode());
        if (storeMapper.findByStoreNum(req.storeNum()) != null) {
            throw new BizException(ErrorCode.STATE_CONFLICT, "仓库编码已存在");
        }
        StoreEntity e = new StoreEntity();
        e.setCompanyCode(req.companyCode());
        e.setStoreNum(req.storeNum());
        e.setStoreName(req.storeName());
        e.setArea(req.area());
        e.setDangerLevel(req.dangerLevel());
        e.setQuotaDosage(req.quotaDosage());
        e.setQuotaPeople(req.quotaPeople());
        storeMapper.insert(e);
        return e.getId();
    }

    @Transactional
    public void update(StoreUpdateRequest req) {
        StoreEntity e = storeMapper.findById(req.id());
        if (e == null) throw new BizException(ErrorCode.NOT_FOUND, "仓库不存在");
        dataScopeService.assertCompanyAllowed(e.getCompanyCode());
        dataScopeService.assertCompanyAllowed(req.companyCode());
        StoreEntity byNum = storeMapper.findByStoreNum(req.storeNum());
        if (byNum != null && !byNum.getId().equals(req.id())) {
            throw new BizException(ErrorCode.STATE_CONFLICT, "仓库编码已存在");
        }
        e.setCompanyCode(req.companyCode());
        e.setStoreNum(req.storeNum());
        e.setStoreName(req.storeName());
        e.setArea(req.area());
        e.setDangerLevel(req.dangerLevel());
        e.setQuotaDosage(req.quotaDosage());
        e.setQuotaPeople(req.quotaPeople());
        storeMapper.update(e);
    }

    @Transactional
    public void delete(Long id) {
        StoreEntity e = storeMapper.findById(id);
        if (e == null) throw new BizException(ErrorCode.NOT_FOUND, "仓库不存在");
        dataScopeService.assertCompanyAllowed(e.getCompanyCode());
        int n = storeMapper.deleteById(id);
        if (n <= 0) throw new BizException(ErrorCode.NOT_FOUND, "仓库不存在");
    }

    public PageResponse<StoreVO> list(String keyword, int page, int pageSize) {
        int offset = (page - 1) * pageSize;
        List<String> scope = dataScopeService.currentCompanyCodesOrAll();
        List<StoreEntity> list = storeMapper.list(keyword, scope, offset, pageSize);
        long total = storeMapper.count(keyword, scope);
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
                String companyCode = CsvImportUtils.firstNonBlank(row, "companyCode", "企业编码");
                String storeNum = CsvImportUtils.firstNonBlank(row, "storeNum", "仓库编号", "仓库编码");
                String storeName = CsvImportUtils.firstNonBlank(row, "storeName", "仓库名称");
                if (companyCode.isBlank() || storeNum.isBlank() || storeName.isBlank()) {
                    throw new BizException(ErrorCode.PARAM_INVALID, "企业编码/仓库编号/名称不能为空");
                }
                dataScopeService.assertCompanyAllowed(companyCode);
                Double area = parseDouble(CsvImportUtils.firstNonBlank(row, "area", "仓库面积（m²）", "仓库面积"));
                String dangerLevel = CsvImportUtils.firstNonBlank(row, "dangerLevel", "危险等级");
                Double quotaDosage = parseDouble(CsvImportUtils.firstNonBlank(row, "quotaDosage", "核定药量（kg）", "核定药量"));
                Integer quotaPeople = parseInt(CsvImportUtils.firstNonBlank(row, "quotaPeople", "核定人数（人）", "核定人数"));

                StoreEntity existing = storeMapper.findByStoreNum(storeNum);
                if (existing == null) {
                    StoreEntity e = new StoreEntity();
                    e.setCompanyCode(companyCode);
                    e.setStoreNum(storeNum);
                    e.setStoreName(storeName);
                    e.setArea(area);
                    e.setDangerLevel(blankToNull(dangerLevel));
                    e.setQuotaDosage(quotaDosage);
                    e.setQuotaPeople(quotaPeople);
                    storeMapper.insert(e);
                } else {
                    dataScopeService.assertCompanyAllowed(existing.getCompanyCode());
                    existing.setCompanyCode(companyCode);
                    existing.setStoreName(storeName);
                    existing.setArea(area);
                    existing.setDangerLevel(blankToNull(dangerLevel));
                    existing.setQuotaDosage(quotaDosage);
                    existing.setQuotaPeople(quotaPeople);
                    storeMapper.update(existing);
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

    private Double parseDouble(String value) {
        if (value == null || value.isBlank()) return null;
        try {
            return Double.valueOf(value.trim());
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

    private StoreVO toVo(StoreEntity e) {
        return new StoreVO(e.getId(), e.getCompanyCode(), e.getStoreNum(), e.getStoreName(), e.getArea(), e.getDangerLevel(), e.getQuotaDosage(), e.getQuotaPeople());
    }
}
