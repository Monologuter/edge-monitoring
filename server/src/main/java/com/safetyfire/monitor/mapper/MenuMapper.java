package com.safetyfire.monitor.mapper;

import com.safetyfire.monitor.domain.vo.MenuVO;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * 菜单数据访问层。
 */
@Mapper
public interface MenuMapper {
    /**
     * 查询用户可见菜单（按角色聚合）。
     */
    List<MenuVO> listMenusByUserId(@Param("userId") Long userId);
}

