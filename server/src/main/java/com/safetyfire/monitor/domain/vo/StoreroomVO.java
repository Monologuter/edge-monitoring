package com.safetyfire.monitor.domain.vo;

/**
 * 库房视图对象。
 */
public record StoreroomVO(
        Long id,
        String storeNum,
        String storeroomNum,
        String storeroomName,
        Double area,
        String dangerLevel,
        Double quotaDosage,
        Integer quotaPeople
) {
}

