package com.safetyfire.monitor.config;

import com.safetyfire.monitor.controller.AiBoxHttpController;
import com.safetyfire.monitor.domain.vo.AiBoxSimpleResponse;
import jakarta.servlet.http.HttpServletRequest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.annotation.Order;
import org.springframework.http.HttpStatus;
import org.springframework.validation.BindException;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestControllerAdvice;

/**
 * AI盒子对接接口专用异常处理：保证返回格式满足厂商对接要求（code/message）。
 *
 * 注意：仅作用于 {@link AiBoxHttpController}，避免影响系统其他接口的统一响应体。
 */
@RestControllerAdvice(assignableTypes = AiBoxHttpController.class)
@Order(-1)
public class AiBoxHttpExceptionHandler {
    private static final Logger log = LoggerFactory.getLogger(AiBoxHttpExceptionHandler.class);

    @ExceptionHandler({MethodArgumentNotValidException.class, BindException.class})
    @ResponseStatus(HttpStatus.OK)
    public AiBoxSimpleResponse handleValid(Exception e, HttpServletRequest request) {
        log.warn("AI盒子参数校验失败 uri={} err={}", request.getRequestURI(), e.getMessage());
        return new AiBoxSimpleResponse("400", "param invalid");
    }

    @ExceptionHandler(Exception.class)
    @ResponseStatus(HttpStatus.OK)
    public AiBoxSimpleResponse handleAny(Exception e, HttpServletRequest request) {
        log.error("AI盒子接口异常 uri={}", request.getRequestURI(), e);
        return new AiBoxSimpleResponse("500", "error");
    }
}

