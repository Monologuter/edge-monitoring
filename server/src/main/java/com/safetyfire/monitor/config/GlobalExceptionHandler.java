package com.safetyfire.monitor.config;

import com.safetyfire.monitor.common.ApiResponse;
import com.safetyfire.monitor.common.BizException;
import com.safetyfire.monitor.common.ErrorCode;
import jakarta.servlet.http.HttpServletRequest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.validation.BindException;
import org.springframework.web.ErrorResponseException;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestControllerAdvice;

/**
 * 全局异常处理：统一响应体 + 统一日志策略。
 */
@RestControllerAdvice
public class GlobalExceptionHandler {
    private static final Logger log = LoggerFactory.getLogger(GlobalExceptionHandler.class);

    @ExceptionHandler(BizException.class)
    @ResponseStatus(HttpStatus.OK)
    public ApiResponse<Void> handleBiz(BizException e, HttpServletRequest request) {
        log.warn("业务异常 uri={} msg={}", request.getRequestURI(), e.getMessage());
        return ApiResponse.fail(e.getErrorCode(), e.getMessage());
    }

    @ExceptionHandler({MethodArgumentNotValidException.class, BindException.class})
    @ResponseStatus(HttpStatus.OK)
    public ApiResponse<Void> handleValid(Exception e, HttpServletRequest request) {
        log.warn("参数校验失败 uri={} err={}", request.getRequestURI(), e.getMessage());
        return ApiResponse.fail(ErrorCode.PARAM_INVALID, "参数错误");
    }

    @ExceptionHandler(ErrorResponseException.class)
    @ResponseStatus(HttpStatus.OK)
    public ApiResponse<Void> handleErrorResponse(ErrorResponseException e, HttpServletRequest request) {
        log.warn("请求异常 uri={} status={} msg={}", request.getRequestURI(), e.getStatusCode().value(), e.getMessage());
        return ApiResponse.fail(ErrorCode.SYSTEM_ERROR, "请求失败");
    }

    @ExceptionHandler(Exception.class)
    @ResponseStatus(HttpStatus.OK)
    public ApiResponse<Void> handleAny(Exception e, HttpServletRequest request) {
        log.error("系统异常 uri={}", request.getRequestURI(), e);
        return ApiResponse.fail(ErrorCode.SYSTEM_ERROR, "系统繁忙，请稍后重试");
    }
}

