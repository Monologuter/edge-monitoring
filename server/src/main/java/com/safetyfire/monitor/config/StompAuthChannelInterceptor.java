package com.safetyfire.monitor.config;

import com.safetyfire.monitor.security.AuthUser;
import com.safetyfire.monitor.security.JwtService;
import org.springframework.messaging.Message;
import org.springframework.messaging.MessageChannel;
import org.springframework.messaging.simp.stomp.StompCommand;
import org.springframework.messaging.simp.stomp.StompHeaderAccessor;
import org.springframework.messaging.support.ChannelInterceptor;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.stereotype.Component;

import java.security.Principal;
import java.util.ArrayList;
import java.util.List;

/**
 * WebSocket STOMP 鉴权拦截：要求 CONNECT 时携带 Authorization: Bearer xxx。
 */
@Component
public class StompAuthChannelInterceptor implements ChannelInterceptor {
    private final JwtService jwtService;

    public StompAuthChannelInterceptor(JwtService jwtService) {
        this.jwtService = jwtService;
    }

    @Override
    public Message<?> preSend(Message<?> message, MessageChannel channel) {
        StompHeaderAccessor accessor = StompHeaderAccessor.wrap(message);
        if (StompCommand.CONNECT.equals(accessor.getCommand())) {
            String auth = accessor.getFirstNativeHeader("Authorization");
            if (auth == null || !auth.startsWith("Bearer ")) {
                throw new IllegalArgumentException("Missing Authorization");
            }
            String token = auth.substring("Bearer ".length()).trim();
            AuthUser user = jwtService.parseAccessToken(token);

            List<SimpleGrantedAuthority> authorities = new ArrayList<>();
            for (String r : user.roles()) authorities.add(new SimpleGrantedAuthority("ROLE_" + r));
            for (String p : user.permissions()) authorities.add(new SimpleGrantedAuthority(p));
            Principal principal = new UsernamePasswordAuthenticationToken(user.username(), null, authorities);
            accessor.setUser(principal);
            accessor.getSessionAttributes().put("sf.userId", user.userId());
        }
        if (StompCommand.SUBSCRIBE.equals(accessor.getCommand())) {
            if (accessor.getUser() == null) {
                throw new IllegalArgumentException("Unauthenticated");
            }
        }
        return message;
    }
}

