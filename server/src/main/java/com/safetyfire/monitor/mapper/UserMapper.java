package com.safetyfire.monitor.mapper;

import com.safetyfire.monitor.domain.entity.UserEntity;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * 用户数据访问层。
 */
@Mapper
public interface UserMapper {
    UserEntity findByUsername(@Param("username") String username);

    UserEntity findById(@Param("id") Long id);

    void insert(UserEntity user);

    List<String> listRolesByUserId(@Param("userId") Long userId);

    void updatePassword(@Param("id") Long id, @Param("passwordHash") String passwordHash);

    List<UserEntity> listUsers(@Param("keyword") String keyword, @Param("offset") int offset, @Param("pageSize") int pageSize);

    long countUsers(@Param("keyword") String keyword);

    int deleteById(@Param("id") Long id);

    int updateBasic(@Param("id") Long id, @Param("displayName") String displayName, @Param("enabled") Integer enabled);
}
