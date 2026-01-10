package com.safetyfire.monitor.domain.vo;

/**
 * 摄像头视图对象。
 */
public record CameraVO(
        Long id,
        String companyCode,
        String cameraCode,
        String cameraName,
        String streamUrl,
        String locationName,
        String storeNum,
        String storeroomNum,
        Integer enabled
) {
}

