package com.safetyfire.monitor.security;

import cn.hutool.core.util.StrUtil;
import com.safetyfire.monitor.common.BizException;
import com.safetyfire.monitor.common.ErrorCode;
import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.security.Keys;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import javax.crypto.SecretKey;
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.util.Date;
import java.util.List;
import java.util.Map;

/**
 * JWT 生成与解析。
 */
@Component
public class JwtService {
    private final SecretKey key;
    private final long accessTtlSeconds;
    private final long refreshTtlSeconds;

    public JwtService(
            @Value("${app.jwt.secret}") String secret,
            @Value("${app.jwt.access-ttl-seconds}") long accessTtlSeconds,
            @Value("${app.jwt.refresh-ttl-seconds}") long refreshTtlSeconds
    ) {
        if (StrUtil.isBlank(secret) || secret.length() < 32) {
            throw new IllegalArgumentException("app.jwt.secret 长度至少 32 位");
        }
        this.key = Keys.hmacShaKeyFor(secret.getBytes(StandardCharsets.UTF_8));
        this.accessTtlSeconds = accessTtlSeconds;
        this.refreshTtlSeconds = refreshTtlSeconds;
    }

    public long getAccessTtlSeconds() {
        return accessTtlSeconds;
    }

    public long getRefreshTtlSeconds() {
        return refreshTtlSeconds;
    }

    public String createAccessToken(AuthUser user, String jti) {
        Instant now = Instant.now();
        Instant exp = now.plusSeconds(accessTtlSeconds);
        return Jwts.builder()
                .id(jti)
                .subject(String.valueOf(user.userId()))
                .issuedAt(Date.from(now))
                .expiration(Date.from(exp))
                .claims(Map.of(
                        "typ", "access",
                        "username", user.username(),
                        "displayName", user.displayName(),
                        "roles", user.roles(),
                        "perms", user.permissions()
                ))
                .signWith(key)
                .compact();
    }

    public String createRefreshToken(AuthUser user, String jti) {
        Instant now = Instant.now();
        Instant exp = now.plusSeconds(refreshTtlSeconds);
        return Jwts.builder()
                .id(jti)
                .subject(String.valueOf(user.userId()))
                .issuedAt(Date.from(now))
                .expiration(Date.from(exp))
                .claims(Map.of("typ", "refresh"))
                .signWith(key)
                .compact();
    }

    public AuthUser parseAccessToken(String token) {
        Claims claims = parse(token);
        if (!"access".equals(claims.get("typ", String.class))) {
            throw new BizException(ErrorCode.UNAUTHORIZED, "Token 类型错误");
        }
        Long userId = Long.valueOf(claims.getSubject());
        String username = claims.get("username", String.class);
        String displayName = claims.get("displayName", String.class);
        @SuppressWarnings("unchecked")
        List<String> roles = (List<String>) claims.get("roles", List.class);
        @SuppressWarnings("unchecked")
        List<String> perms = (List<String>) claims.get("perms", List.class);
        return new AuthUser(
                userId,
                username,
                displayName,
                roles == null ? List.of() : roles,
                perms == null ? List.of() : perms
        );
    }

    public Claims parseRefreshTokenClaims(String token) {
        Claims claims = parse(token);
        if (!"refresh".equals(claims.get("typ", String.class))) {
            throw new BizException(ErrorCode.UNAUTHORIZED, "Token 类型错误");
        }
        return claims;
    }

    private Claims parse(String token) {
        try {
            return Jwts.parser()
                    .verifyWith(key)
                    .build()
                    .parseSignedClaims(token)
                    .getPayload();
        } catch (io.jsonwebtoken.ExpiredJwtException e) {
            throw new BizException(ErrorCode.TOKEN_EXPIRED, "Token 已过期");
        } catch (Exception e) {
            throw new BizException(ErrorCode.UNAUTHORIZED, "Token 无效");
        }
    }
}
