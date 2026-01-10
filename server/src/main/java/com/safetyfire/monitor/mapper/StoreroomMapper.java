package com.safetyfire.monitor.mapper;

import com.safetyfire.monitor.domain.entity.StoreroomEntity;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * 库房访问层。
 */
@Mapper
public interface StoreroomMapper {
    StoreroomEntity findById(@Param("id") Long id);

    StoreroomEntity findByStoreroomNum(@Param("storeroomNum") String storeroomNum);

    void insert(StoreroomEntity e);

    int update(StoreroomEntity e);

    int deleteById(@Param("id") Long id);

    List<StoreroomEntity> list(@Param("storeNum") String storeNum, @Param("keyword") String keyword,
                               @Param("companyCodes") List<String> companyCodes,
                               @Param("offset") int offset, @Param("pageSize") int pageSize);

    long count(@Param("storeNum") String storeNum, @Param("keyword") String keyword, @Param("companyCodes") List<String> companyCodes);
}
