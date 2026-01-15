package com.safetyfire.monitor.controller;

import com.safetyfire.monitor.common.ApiResponse;
import com.safetyfire.monitor.config.Audit;
import com.safetyfire.monitor.domain.dto.ChangePasswordRequest;
import com.safetyfire.monitor.domain.dto.LoginRequest;
import com.safetyfire.monitor.domain.dto.RefreshRequest;
import com.safetyfire.monitor.domain.vo.TokenVO;
import com.safetyfire.monitor.domain.vo.UserVO;
import com.safetyfire.monitor.service.AuthService;
import com.safetyfire.monitor.service.CaptchaService;
import jakarta.validation.Valid;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

/**
 * 认证接口。
 */
@RestController
@RequestMapping("/api/v1/auth")
public class AuthController {
    private final AuthService authService;
    private final CaptchaService captchaService;

    public AuthController(AuthService authService, CaptchaService captchaService) {
        this.authService = authService;
        this.captchaService = captchaService;
    }

    @PostMapping("/login")
    @Audit(action = "auth.login")
    public ApiResponse<TokenVO> login(@Valid @RequestBody LoginRequest req) {
        return ApiResponse.ok(authService.login(req.username(), req.password(), req.captchaId(), req.captchaCode()));
    }

    @GetMapping("/captcha")
    public ApiResponse<CaptchaService.CaptchaResult> captcha() {
        return ApiResponse.ok(captchaService.generate());
    }

    @PostMapping("/refresh")
    public ApiResponse<TokenVO> refresh(@Valid @RequestBody RefreshRequest req) {
        return ApiResponse.ok(authService.refresh(req.refreshToken()));
    }

    @GetMapping("/me")
    public ApiResponse<UserVO> me() {
        return ApiResponse.ok(authService.me());
    }

    @PostMapping("/change-password")
    @PreAuthorize("isAuthenticated()")
    @Audit(action = "auth.changePassword")
    public ApiResponse<Void> changePassword(@Valid @RequestBody ChangePasswordRequest req) {
        authService.changePassword(req);
        return ApiResponse.ok(null);
    }
}
