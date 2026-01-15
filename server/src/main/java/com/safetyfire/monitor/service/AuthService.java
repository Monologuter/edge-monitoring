package com.safetyfire.monitor.service;

import cn.hutool.core.lang.UUID;
import com.safetyfire.monitor.common.BizException;
import com.safetyfire.monitor.common.ErrorCode;
import com.safetyfire.monitor.config.AuthProperties;
import com.safetyfire.monitor.domain.entity.UserEntity;
import com.safetyfire.monitor.domain.dto.ChangePasswordRequest;
import com.safetyfire.monitor.domain.vo.TokenVO;
import com.safetyfire.monitor.domain.vo.MenuVO;
import com.safetyfire.monitor.domain.vo.UserVO;
import com.safetyfire.monitor.mapper.MenuMapper;
import com.safetyfire.monitor.mapper.PermissionMapper;
import com.safetyfire.monitor.mapper.UserCompanyScopeMapper;
import com.safetyfire.monitor.mapper.UserMapper;
import com.safetyfire.monitor.security.AuthUser;
import com.safetyfire.monitor.security.AuthUserHolder;
import com.safetyfire.monitor.security.JwtService;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

import java.time.Duration;
import java.util.List;
import java.util.Set;

/**
 * 认证服务：登录、刷新、当前用户。
 */
@Service
public class AuthService {
    private final UserMapper userMapper;
    private final PermissionMapper permissionMapper;
    private final MenuMapper menuMapper;
    private final UserCompanyScopeMapper userCompanyScopeMapper;
    private final PasswordEncoder passwordEncoder;
    private final JwtService jwtService;
    private final StringRedisTemplate redisTemplate;
    private final CaptchaService captchaService;
    private final AuthProperties authProperties;

    public AuthService(UserMapper userMapper, PermissionMapper permissionMapper, MenuMapper menuMapper,
                       UserCompanyScopeMapper userCompanyScopeMapper,
                       PasswordEncoder passwordEncoder, JwtService jwtService, StringRedisTemplate redisTemplate,
                       CaptchaService captchaService, AuthProperties authProperties) {
        this.userMapper = userMapper;
        this.permissionMapper = permissionMapper;
        this.menuMapper = menuMapper;
        this.userCompanyScopeMapper = userCompanyScopeMapper;
        this.passwordEncoder = passwordEncoder;
        this.jwtService = jwtService;
        this.redisTemplate = redisTemplate;
        this.captchaService = captchaService;
        this.authProperties = authProperties;
    }

    public TokenVO login(String username, String password, String captchaId, String captchaCode) {
        if (authProperties.isCaptchaEnabled()) {
            boolean ok = captchaService.validate(captchaId, captchaCode);
            if (!ok) {
                throw new BizException(ErrorCode.PARAM_INVALID, "验证码错误或已过期");
            }
        }
        UserEntity user = userMapper.findByUsername(username);
        if (user == null || user.getEnabled() == null || user.getEnabled() != 1) {
            throw new BizException(ErrorCode.LOGIN_FAILED, "账号或密码错误");
        }
        if (!passwordEncoder.matches(password, user.getPasswordHash())) {
            throw new BizException(ErrorCode.LOGIN_FAILED, "账号或密码错误");
        }

        List<String> roles = userMapper.listRolesByUserId(user.getId());
        List<String> perms = permissionMapper.listPermKeysByUserId(user.getId());
        AuthUser authUser = new AuthUser(user.getId(), user.getUsername(), user.getDisplayName(), roles, perms);

        String accessJti = UUID.fastUUID().toString(true);
        String refreshJti = UUID.fastUUID().toString(true);
        String accessToken = jwtService.createAccessToken(authUser, accessJti);
        String refreshToken = jwtService.createRefreshToken(authUser, refreshJti);

        String key = refreshKey(refreshJti);
        redisTemplate.opsForValue().set(key, String.valueOf(user.getId()), Duration.ofSeconds(jwtService.getRefreshTtlSeconds()));
        // 记录用户下的 refresh jti，用于改密/注销时集中失效
        redisTemplate.opsForSet().add(userRefreshSetKey(user.getId()), refreshJti);
        redisTemplate.expire(userRefreshSetKey(user.getId()), Duration.ofSeconds(jwtService.getRefreshTtlSeconds()));

        return new TokenVO(accessToken, refreshToken, jwtService.getAccessTtlSeconds());
    }

    public TokenVO refresh(String refreshToken) {
        var claims = jwtService.parseRefreshTokenClaims(refreshToken);
        String refreshJti = claims.getId();
        String userId = claims.getSubject();

        String key = refreshKey(refreshJti);
        String storedUserId = redisTemplate.opsForValue().get(key);
        if (storedUserId == null || !storedUserId.equals(userId)) {
            throw new BizException(ErrorCode.UNAUTHORIZED, "刷新令牌无效");
        }

        UserEntity user = userMapper.findById(Long.valueOf(userId));
        if (user == null || user.getEnabled() == null || user.getEnabled() != 1) {
            throw new BizException(ErrorCode.UNAUTHORIZED, "用户不存在或已禁用");
        }

        List<String> roles = userMapper.listRolesByUserId(user.getId());
        List<String> perms = permissionMapper.listPermKeysByUserId(user.getId());
        AuthUser authUser = new AuthUser(user.getId(), user.getUsername(), user.getDisplayName(), roles, perms);

        redisTemplate.delete(key);
        redisTemplate.opsForSet().remove(userRefreshSetKey(user.getId()), refreshJti);

        String accessJti = UUID.fastUUID().toString(true);
        String newRefreshJti = UUID.fastUUID().toString(true);
        String access = jwtService.createAccessToken(authUser, accessJti);
        String refresh = jwtService.createRefreshToken(authUser, newRefreshJti);
        redisTemplate.opsForValue().set(refreshKey(newRefreshJti), String.valueOf(user.getId()),
                Duration.ofSeconds(jwtService.getRefreshTtlSeconds()));
        redisTemplate.opsForSet().add(userRefreshSetKey(user.getId()), newRefreshJti);
        redisTemplate.expire(userRefreshSetKey(user.getId()), Duration.ofSeconds(jwtService.getRefreshTtlSeconds()));

        return new TokenVO(access, refresh, jwtService.getAccessTtlSeconds());
    }

    public UserVO me() {
        AuthUser user = AuthUserHolder.get();
        if (user == null) {
            throw new BizException(ErrorCode.UNAUTHORIZED, "未登录");
        }
        List<MenuVO> menus = menuMapper.listMenusByUserId(user.userId());
        List<String> companyCodes = userCompanyScopeMapper.listCompanyCodesByUserId(user.userId());
        return new UserVO(user.userId(), user.username(), user.displayName(), user.roles(), user.permissions(), menus, companyCodes);
    }

    public void changePassword(ChangePasswordRequest req) {
        AuthUser auth = AuthUserHolder.get();
        if (auth == null) {
            throw new BizException(ErrorCode.UNAUTHORIZED, "未登录");
        }
        if (!com.safetyfire.monitor.util.PasswordPolicy.isStrong(req.newPassword())) {
            throw new BizException(ErrorCode.PARAM_INVALID, "新密码强度不足：需包含大小写字母与数字/符号");
        }

        UserEntity user = userMapper.findById(auth.userId());
        if (user == null) throw new BizException(ErrorCode.NOT_FOUND, "用户不存在");
        if (!passwordEncoder.matches(req.oldPassword(), user.getPasswordHash())) {
            throw new BizException(ErrorCode.PARAM_INVALID, "旧密码错误");
        }

        user.setPasswordHash(passwordEncoder.encode(req.newPassword()));
        userMapper.updatePassword(user.getId(), user.getPasswordHash());

        // 失效该用户所有 refresh token（强制重新登录）
        String setKey = userRefreshSetKey(user.getId());
        Set<String> jtIs = redisTemplate.opsForSet().members(setKey);
        if (jtIs != null) {
            for (String jti : jtIs) {
                if (jti != null && !jti.isBlank()) {
                    redisTemplate.delete(refreshKey(jti));
                }
            }
        }
        redisTemplate.delete(setKey);
    }

    private static String refreshKey(String jti) {
        return "refresh_token:" + jti;
    }

    private static String userRefreshSetKey(Long userId) {
        return "user_refresh:" + userId;
    }
}
