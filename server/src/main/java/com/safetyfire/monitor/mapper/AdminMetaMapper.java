package com.safetyfire.monitor.mapper;

import com.safetyfire.monitor.domain.vo.MenuVO;
import org.apache.ibatis.annotations.Mapper;

import java.util.List;

/**
 * 管理后台：元数据（权限/菜单）访问层。
 */
@Mapper
public interface AdminMetaMapper {
    List<String> listAllPermissionKeys();

    List<MenuVO> listAllMenus();
}

