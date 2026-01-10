package com.safetyfire.monitor.config;

import cn.hutool.json.JSONUtil;
import com.safetyfire.monitor.common.RequestIdHolder;
import com.safetyfire.monitor.domain.entity.OperationAuditLogEntity;
import com.safetyfire.monitor.mapper.OperationAuditLogMapper;
import com.safetyfire.monitor.security.AuthUser;
import com.safetyfire.monitor.security.AuthUserHolder;
import com.safetyfire.monitor.util.IpUtils;
import com.safetyfire.monitor.util.MaskUtils;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.stereotype.Component;
import org.springframework.web.method.HandlerMethod;
import org.springframework.web.servlet.HandlerInterceptor;

import java.util.HashMap;
import java.util.Map;

/**
 * 操作审计拦截器：记录关键接口的请求摘要与响应 code。
 */
@Component
public class AuditInterceptor implements HandlerInterceptor {
    private static final String ATTR_START = "sf.audit.start";

    private final OperationAuditLogMapper auditLogMapper;

    public AuditInterceptor(OperationAuditLogMapper auditLogMapper) {
        this.auditLogMapper = auditLogMapper;
    }

    @Override
    public boolean preHandle(HttpServletRequest request, HttpServletResponse response, Object handler) {
        request.setAttribute(ATTR_START, System.currentTimeMillis());
        return true;
    }

    @Override
    public void afterCompletion(HttpServletRequest request, HttpServletResponse response, Object handler, Exception ex) {
        if (!(handler instanceof HandlerMethod hm)) return;
        Audit audit = hm.getMethodAnnotation(Audit.class);
        if (audit == null) return;

        Long start = (Long) request.getAttribute(ATTR_START);
        long cost = start == null ? -1 : (System.currentTimeMillis() - start);

        AuthUser user = AuthUserHolder.get();
        String username = user == null ? null : user.username();

        Integer code = null;
        Object respCode = request.getAttribute(ApiResponseCaptureAdvice.ATTR_CODE);
        if (respCode instanceof Integer i) code = i;

        OperationAuditLogEntity e = new OperationAuditLogEntity();
        e.setRequestId(RequestIdHolder.get());
        e.setUsername(username);
        e.setUri(request.getRequestURI());
        e.setMethod(request.getMethod());
        e.setIp(IpUtils.getClientIp(request));
        e.setAction(audit.action());
        e.setResponseCode(code);
        e.setCostMs(cost);

        // 请求摘要：避免写入大字段与敏感字段
        Map<String, Object> summary = new HashMap<>();
        request.getParameterMap().forEach((k, v) -> {
            if ("password".equalsIgnoreCase(k)) {
                summary.put(k, MaskUtils.maskPassword("x"));
            } else {
                summary.put(k, v == null ? null : String.join(",", v));
            }
        });
        e.setRequestJson(JSONUtil.toJsonStr(summary));

        auditLogMapper.insert(e);
    }
}

