package com.safetyfire.monitor.config;

import com.safetyfire.monitor.common.ApiResponse;
import com.safetyfire.monitor.common.ErrorCode;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.time.Duration;

import static com.safetyfire.monitor.util.IpUtils.getClientIp;

/**
 * 简单限流：对登录接口按 IP 每分钟限频，避免暴力破解。
 */
@Component
public class RateLimitFilter extends OncePerRequestFilter {
    private final StringRedisTemplate redisTemplate;

    public RateLimitFilter(StringRedisTemplate redisTemplate) {
        this.redisTemplate = redisTemplate;
    }

    @Override
    protected boolean shouldNotFilter(HttpServletRequest request) {
        return !("/api/v1/auth/login".equals(request.getRequestURI()));
    }

    @Override
    protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain filterChain)
            throws ServletException, IOException {
        String ip = getClientIp(request);
        String key = "rl:login:" + ip;
        Long c = redisTemplate.opsForValue().increment(key);
        if (c != null && c == 1L) {
            redisTemplate.expire(key, Duration.ofSeconds(60));
        }
        if (c != null && c > 10) {
            response.setStatus(200);
            response.setContentType(MediaType.APPLICATION_JSON_VALUE);
            response.getOutputStream().write(
                    ("""
                            {"code":%d,"message":"请求过于频繁，请稍后重试","data":null,"requestId":null,"timestamp":%d}
                            """.formatted(ErrorCode.RATE_LIMITED.code(), System.currentTimeMillis()))
                            .getBytes(StandardCharsets.UTF_8)
            );
            return;
        }
        filterChain.doFilter(request, response);
    }
}

