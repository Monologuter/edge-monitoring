package com.safetyfire.monitor.config;

import com.safetyfire.monitor.common.ApiResponse;
import jakarta.servlet.http.HttpServletRequest;
import org.springframework.core.MethodParameter;
import org.springframework.http.MediaType;
import org.springframework.http.converter.HttpMessageConverter;
import org.springframework.http.server.ServerHttpRequest;
import org.springframework.http.server.ServerHttpResponse;
import org.springframework.http.server.ServletServerHttpRequest;
import org.springframework.web.bind.annotation.RestControllerAdvice;
import org.springframework.web.servlet.mvc.method.annotation.ResponseBodyAdvice;

/**
 * 捕获 ApiResponse 的 code，写入 request attribute 便于审计落库。
 */
@RestControllerAdvice
public class ApiResponseCaptureAdvice implements ResponseBodyAdvice<Object> {
    public static final String ATTR_CODE = "sf.response.code";

    @Override
    public boolean supports(MethodParameter returnType, Class<? extends HttpMessageConverter<?>> converterType) {
        return true;
    }

    @Override
    public Object beforeBodyWrite(Object body, MethodParameter returnType, MediaType selectedContentType,
                                  Class<? extends HttpMessageConverter<?>> selectedConverterType,
                                  ServerHttpRequest request, ServerHttpResponse response) {
        if (body instanceof ApiResponse<?> api) {
            if (request instanceof ServletServerHttpRequest sr) {
                HttpServletRequest r = sr.getServletRequest();
                r.setAttribute(ATTR_CODE, api.code());
            }
        }
        return body;
    }
}

