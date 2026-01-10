package com.safetyfire.monitor.mapper;

import com.safetyfire.monitor.domain.entity.CarInoutRecordEntity;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * 车辆出入记录访问层。
 */
@Mapper
public interface CarInoutRecordMapper {
    void insert(CarInoutRecordEntity e);

    List<CarInoutRecordEntity> listByCompanyCodes(@Param("companyCodes") List<String> companyCodes,
                                                  @Param("offset") int offset, @Param("pageSize") int pageSize);

    long countByCompanyCodes(@Param("companyCodes") List<String> companyCodes);
}
