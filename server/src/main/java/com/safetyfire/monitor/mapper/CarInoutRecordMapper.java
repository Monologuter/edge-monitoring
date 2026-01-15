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

    CarInoutRecordEntity findById(@Param("id") Long id);

    int update(CarInoutRecordEntity e);

    int deleteById(@Param("id") Long id);

    List<CarInoutRecordEntity> listByCompanyCodes(@Param("companyCodes") List<String> companyCodes,
                                                  @Param("offset") int offset, @Param("pageSize") int pageSize);

    long countByCompanyCodes(@Param("companyCodes") List<String> companyCodes);

    long countLatestIn(@Param("companyCodes") List<String> companyCodes);

    long countTodayIn(@Param("companyCodes") List<String> companyCodes,
                      @Param("startTime") long startTime,
                      @Param("endTime") long endTime);

    long countTodayOut(@Param("companyCodes") List<String> companyCodes,
                       @Param("startTime") long startTime,
                       @Param("endTime") long endTime);
}
