package com.safetyfire.monitor.service;

import com.safetyfire.monitor.common.BizException;
import com.safetyfire.monitor.common.ErrorCode;
import com.safetyfire.monitor.common.PageResponse;
import com.safetyfire.monitor.domain.dto.StoreroomCreateRequest;
import com.safetyfire.monitor.domain.dto.StoreroomUpdateRequest;
import com.safetyfire.monitor.domain.entity.StoreroomEntity;
import com.safetyfire.monitor.domain.vo.StoreroomVO;
import com.safetyfire.monitor.mapper.StoreroomMapper;
import com.safetyfire.monitor.mapper.StoreMapper;
import com.safetyfire.monitor.security.DataScopeService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

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

    private StoreroomVO toVo(StoreroomEntity e) {
        return new StoreroomVO(e.getId(), e.getStoreNum(), e.getStoreroomNum(), e.getStoreroomName(), e.getArea(), e.getDangerLevel(), e.getQuotaDosage(), e.getQuotaPeople());
    }
}
