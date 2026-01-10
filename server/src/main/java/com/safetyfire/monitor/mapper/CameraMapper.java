package com.safetyfire.monitor.mapper;

import com.safetyfire.monitor.domain.entity.CameraEntity;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * 摄像头接入访问层。
 */
@Mapper
public interface CameraMapper {
    CameraEntity findById(@Param("id") Long id);

    CameraEntity findByCameraCode(@Param("cameraCode") String cameraCode);

    void insert(CameraEntity e);

    int update(CameraEntity e);

    int deleteById(@Param("id") Long id);

    List<CameraEntity> list(@Param("keyword") String keyword, @Param("companyCodes") List<String> companyCodes,
                            @Param("offset") int offset, @Param("pageSize") int pageSize);

    long count(@Param("keyword") String keyword, @Param("companyCodes") List<String> companyCodes);
}
