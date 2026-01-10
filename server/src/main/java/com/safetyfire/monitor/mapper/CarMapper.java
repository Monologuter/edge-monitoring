package com.safetyfire.monitor.mapper;

import com.safetyfire.monitor.domain.entity.CarEntity;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * 车辆档案访问层。
 */
@Mapper
public interface CarMapper {
    CarEntity findById(@Param("id") Long id);

    CarEntity findByPlate(@Param("plate") String plate);

    void insert(CarEntity e);

    int update(CarEntity e);

    int deleteById(@Param("id") Long id);

    List<CarEntity> list(@Param("keyword") String keyword, @Param("companyCodes") List<String> companyCodes,
                         @Param("offset") int offset, @Param("pageSize") int pageSize);

    long count(@Param("keyword") String keyword, @Param("companyCodes") List<String> companyCodes);
}
