package com.safetyfire.monitor.service;

import com.safetyfire.monitor.common.BizException;
import com.safetyfire.monitor.common.ErrorCode;
import com.safetyfire.monitor.common.PageResponse;
import com.safetyfire.monitor.domain.dto.StoreroomCreateRequest;
import com.safetyfire.monitor.domain.dto.StoreroomUpdateRequest;
import com.safetyfire.monitor.domain.entity.StoreroomEntity;
import com.safetyfire.monitor.domain.vo.StoreroomVO;
import com.safetyfire.monitor.domain.vo.ImportResultVO;
import com.safetyfire.monitor.mapper.StoreroomMapper;
import com.safetyfire.monitor.mapper.StoreMapper;
import com.safetyfire.monitor.security.DataScopeService;
import com.safetyfire.monitor.util.CsvImportUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;
import java.util.Map;

/**
 * 库房服务（FR-04）。
 */
@Service
public class StoreroomService {
    private final StoreroomMapper storeroomMapper;
    private final StoreMapper storeMapper;
    private final DataScopeService dataScopeService;

    public StoreroomService(StoreroomMapper storeroomMapper, StoreMapper storeMapper, DataScopeService dataScopeService) {
        this.storeroomMapper = storeroomMapper;
        this.storeMapper = storeMapper;
        this.dataScopeService = dataScopeService;
    }

    @Transactional
    public Long create(StoreroomCreateRequest req) {
        // 校验仓库所属企业是否在数据范围内
        var store = storeMapper.findByStoreNum(req.storeNum());
        if (store == null) throw new BizException(ErrorCode.NOT_FOUND, "仓库不存在");
        dataScopeService.assertCompanyAllowed(store.getCompanyCode());
        if (storeroomMapper.findByStoreroomNum(req.storeroomNum()) != null) {
            throw new BizException(ErrorCode.STATE_CONFLICT, "库房编码已存在");
        }
        StoreroomEntity e = new StoreroomEntity();
        e.setStoreNum(req.storeNum());
        e.setStoreroomNum(req.storeroomNum());
        e.setStoreroomName(req.storeroomName());
        e.setArea(req.area());
        e.setDangerLevel(req.dangerLevel());
        e.setQuotaDosage(req.quotaDosage());
        e.setQuotaPeople(req.quotaPeople());
        storeroomMapper.insert(e);
        return e.getId();
    }

    @Transactional
    public void update(StoreroomUpdateRequest req) {
        StoreroomEntity e = storeroomMapper.findById(req.id());
        if (e == null) throw new BizException(ErrorCode.NOT_FOUND, "库房不存在");
        var store = storeMapper.findByStoreNum(e.getStoreNum());
        if (store == null) throw new BizException(ErrorCode.NOT_FOUND, "仓库不存在");
        dataScopeService.assertCompanyAllowed(store.getCompanyCode());
        var newStore = storeMapper.findByStoreNum(req.storeNum());
        if (newStore == null) throw new BizException(ErrorCode.NOT_FOUND, "仓库不存在");
        dataScopeService.assertCompanyAllowed(newStore.getCompanyCode());
        StoreroomEntity byNum = storeroomMapper.findByStoreroomNum(req.storeroomNum());
        if (byNum != null && !byNum.getId().equals(req.id())) {
            throw new BizException(ErrorCode.STATE_CONFLICT, "库房编码已存在");
        }
        e.setStoreNum(req.storeNum());
        e.setStoreroomNum(req.storeroomNum());
        e.setStoreroomName(req.storeroomName());
        e.setArea(req.area());
        e.setDangerLevel(req.dangerLevel());
        e.setQuotaDosage(req.quotaDosage());
        e.setQuotaPeople(req.quotaPeople());
        storeroomMapper.update(e);
    }

    @Transactional
    public void delete(Long id) {
        StoreroomEntity e = storeroomMapper.findById(id);
        if (e == null) throw new BizException(ErrorCode.NOT_FOUND, "库房不存在");
        var store = storeMapper.findByStoreNum(e.getStoreNum());
        if (store == null) throw new BizException(ErrorCode.NOT_FOUND, "仓库不存在");
        dataScopeService.assertCompanyAllowed(store.getCompanyCode());
        int n = storeroomMapper.deleteById(id);
        if (n <= 0) throw new BizException(ErrorCode.NOT_FOUND, "库房不存在");
    }

    public PageResponse<StoreroomVO> list(String storeNum, String keyword, int page, int pageSize) {
        int offset = (page - 1) * pageSize;
        List<String> scope = dataScopeService.currentCompanyCodesOrAll();
        List<StoreroomEntity> list = storeroomMapper.list(storeNum, keyword, scope, offset, pageSize);
        long total = storeroomMapper.count(storeNum, keyword, scope);
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
                String storeNum = CsvImportUtils.firstNonBlank(row, "storeNum", "仓库编号", "仓库编码");
                String storeroomNum = CsvImportUtils.firstNonBlank(row, "storeroomNum", "库房编号", "库房编码");
                String storeroomName = CsvImportUtils.firstNonBlank(row, "storeroomName", "库房名称");
                if (storeNum.isBlank() || storeroomNum.isBlank() || storeroomName.isBlank()) {
                    throw new BizException(ErrorCode.PARAM_INVALID, "仓库编号/库房编号/名称不能为空");
                }
                var store = storeMapper.findByStoreNum(storeNum);
                if (store == null) throw new BizException(ErrorCode.NOT_FOUND, "仓库不存在");
                dataScopeService.assertCompanyAllowed(store.getCompanyCode());
                Double area = parseDouble(CsvImportUtils.firstNonBlank(row, "area", "库房面积（m²）", "库房面积"));
                String dangerLevel = CsvImportUtils.firstNonBlank(row, "dangerLevel", "危险等级");
                Double quotaDosage = parseDouble(CsvImportUtils.firstNonBlank(row, "quotaDosage", "核定药量（t）", "核定药量"));
                Integer quotaPeople = parseInt(CsvImportUtils.firstNonBlank(row, "quotaPeople", "核定人数（人）", "核定人数"));

                StoreroomEntity existing = storeroomMapper.findByStoreroomNum(storeroomNum);
                if (existing == null) {
                    StoreroomEntity e = new StoreroomEntity();
                    e.setStoreNum(storeNum);
                    e.setStoreroomNum(storeroomNum);
                    e.setStoreroomName(storeroomName);
                    e.setArea(area);
                    e.setDangerLevel(blankToNull(dangerLevel));
                    e.setQuotaDosage(quotaDosage);
                    e.setQuotaPeople(quotaPeople);
                    storeroomMapper.insert(e);
                } else {
                    existing.setStoreNum(storeNum);
                    existing.setStoreroomNum(storeroomNum);
                    existing.setStoreroomName(storeroomName);
                    existing.setArea(area);
                    existing.setDangerLevel(blankToNull(dangerLevel));
                    existing.setQuotaDosage(quotaDosage);
                    existing.setQuotaPeople(quotaPeople);
                    storeroomMapper.update(existing);
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

    private StoreroomVO toVo(StoreroomEntity e) {
        return new StoreroomVO(e.getId(), e.getStoreNum(), e.getStoreroomNum(), e.getStoreroomName(), e.getArea(), e.getDangerLevel(), e.getQuotaDosage(), e.getQuotaPeople());
    }
}
