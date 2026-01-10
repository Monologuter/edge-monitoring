package com.safetyfire.monitor.controller;

import com.safetyfire.monitor.domain.vo.AiBoxSimpleResponse;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * AI 盒子/厂商测试平台连通性探测：
 * - 部分平台会直接对“服务器地址”（不带具体 API）发起 GET 探测
 * - 若返回 403/404，平台可能误判为“连接不上服务器”
 */
@RestController
@RequestMapping({"", "/box", "/api/school/box"})
public class AiBoxHttpPingController {
    @GetMapping({"", "/"})
    public AiBoxSimpleResponse ping() {
        return AiBoxSimpleResponse.ok();
    }
}

