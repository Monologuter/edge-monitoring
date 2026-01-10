package com.safetyfire.monitor.mapper;

import com.safetyfire.monitor.domain.entity.FileObjectEntity;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

/**
 * 文件对象访问层。
 */
@Mapper
public interface FileObjectMapper {
    FileObjectEntity findBySha256(@Param("sha256") String sha256);

    FileObjectEntity findById(@Param("id") Long id);

    void insert(FileObjectEntity e);
}

