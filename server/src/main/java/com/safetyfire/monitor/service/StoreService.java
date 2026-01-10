package com.safetyfire.monitor.service;

import com.safetyfire.monitor.common.BizException;
import com.safetyfire.monitor.common.ErrorCode;
import com.safetyfire.monitor.common.PageResponse;
import com.safetyfire.monitor.domain.dto.StoreCreateRequest;
import com.safetyfire.monitor.domain.dto.StoreUpdateRequest;
import com.safetyfire.monitor.domain.entity.StoreEntity;
import com.safetyfire.monitor.domain.vo.StoreVO;
import com.safetyfire.monitor.mapper.StoreMapper;
import com.safetyfire.monitor.security.DataScopeService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

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

    private StoreVO toVo(StoreEntity e) {
        return new StoreVO(e.getId(), e.getCompanyCode(), e.getStoreNum(), e.getStoreName(), e.getArea(), e.getDangerLevel(), e.getQuotaDosage(), e.getQuotaPeople());
    }
}
