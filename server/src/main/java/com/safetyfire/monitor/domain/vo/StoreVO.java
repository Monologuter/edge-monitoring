package com.safetyfire.monitor.domain.vo;

/**
 * 仓库视图对象。
 */
public record StoreVO(
        Long id,
        String companyCode,
        String storeNum,
        String storeName,
        Double area,
        String dangerLevel,
        Double quotaDosage,
        Integer quotaPeople
) {
}

