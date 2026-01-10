package com.safetyfire.monitor.security;

import cn.hutool.core.util.StrUtil;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.http.HttpHeaders;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * 从 Authorization: Bearer xxx 中解析 JWT，并写入 SecurityContext 与线程上下文。
 */
@Component
public class JwtAuthenticationFilter extends OncePerRequestFilter {
    private final JwtService jwtService;

    public JwtAuthenticationFilter(JwtService jwtService) {
        this.jwtService = jwtService;
    }

    @Override
    protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain filterChain)
            throws ServletException, IOException {
        String auth = request.getHeader(HttpHeaders.AUTHORIZATION);
        if (StrUtil.isNotBlank(auth) && auth.startsWith("Bearer ")) {
            String token = auth.substring("Bearer ".length()).trim();
            if (StrUtil.isNotBlank(token)) {
                AuthUser user = jwtService.parseAccessToken(token);
                AuthUserHolder.set(user);
                List<SimpleGrantedAuthority> authorities = new ArrayList<>();
                authorities.addAll(user.roles().stream()
                        .map(r -> new SimpleGrantedAuthority("ROLE_" + r))
                        .collect(Collectors.toList()));
                authorities.addAll(user.permissions().stream()
                        .map(SimpleGrantedAuthority::new)
                        .collect(Collectors.toList()));
                var authentication = new UsernamePasswordAuthenticationToken(user.username(), null, authorities);
                SecurityContextHolder.getContext().setAuthentication(authentication);
            }
        }

        try {
            filterChain.doFilter(request, response);
        } finally {
            AuthUserHolder.clear();
            SecurityContextHolder.clearContext();
        }
    }
}
