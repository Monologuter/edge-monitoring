package com.safetyfire.monitor.mapper;

import com.safetyfire.monitor.domain.entity.StoreEntity;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * 仓库访问层。
 */
@Mapper
public interface StoreMapper {
    StoreEntity findById(@Param("id") Long id);

    StoreEntity findByStoreNum(@Param("storeNum") String storeNum);

    void insert(StoreEntity e);

    int update(StoreEntity e);

    int deleteById(@Param("id") Long id);

    List<StoreEntity> list(@Param("keyword") String keyword, @Param("companyCodes") List<String> companyCodes,
                           @Param("offset") int offset, @Param("pageSize") int pageSize);

    long count(@Param("keyword") String keyword, @Param("companyCodes") List<String> companyCodes);
}
