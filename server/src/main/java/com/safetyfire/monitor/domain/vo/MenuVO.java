package com.safetyfire.monitor.domain.vo;

/**
 * 菜单视图对象（用于前端动态导航）。
 */
public record MenuVO(Long id, Long parentId, String menuKey, String menuName, String path, String icon, int sortNo) {
}

