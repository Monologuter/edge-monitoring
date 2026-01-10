package com.safetyfire.monitor.security;

import com.safetyfire.monitor.common.BizException;
import com.safetyfire.monitor.common.ErrorCode;
import com.safetyfire.monitor.mapper.UserCompanyScopeMapper;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;

import java.time.Duration;
import java.util.List;

/**
 * 数据权限服务：企业维度 company_code。
 *
 * 规则：
 * - ADMIN 角色：全量数据；
 * - 非 ADMIN：仅允许访问 user_company_scope 中授权的企业编码。
 */
@Service
public class DataScopeService {
    private final UserCompanyScopeMapper userCompanyScopeMapper;
    private final StringRedisTemplate redisTemplate;

    public DataScopeService(UserCompanyScopeMapper userCompanyScopeMapper, StringRedisTemplate redisTemplate) {
        this.userCompanyScopeMapper = userCompanyScopeMapper;
        this.redisTemplate = redisTemplate;
    }

    /**
     * 清理指定用户的数据权限缓存（用于后台变更授权后立即生效）。
     */
    public void evictUserScope(Long userId) {
        if (userId == null) return;
        redisTemplate.delete("scope:user:" + userId);
    }

    /**
     * 返回当前用户可访问的 company_code 列表。
     * - 返回 null 表示“全量”（ADMIN）。
     * - 返回空列表表示“无权限”。
     */
    public List<String> currentCompanyCodesOrAll() {
        AuthUser user = AuthUserHolder.get();
        if (user == null) throw new BizException(ErrorCode.UNAUTHORIZED, "未登录");
        if (user.roles().contains("ADMIN")) return null;

        String cacheKey = "scope:user:" + user.userId();
        String cached = redisTemplate.opsForValue().get(cacheKey);
        if (cached != null) {
            if (cached.isBlank()) return List.of();
            return List.of(cached.split(","));
        }

        List<String> list = userCompanyScopeMapper.listCompanyCodesByUserId(user.userId());
        redisTemplate.opsForValue().set(cacheKey, String.join(",", list), Duration.ofMinutes(5));
        return list;
    }

    public void assertCompanyAllowed(String companyCode) {
        if (companyCode == null || companyCode.isBlank()) {
            throw new BizException(ErrorCode.PARAM_INVALID, "companyCode 不能为空");
        }
        List<String> codes = currentCompanyCodesOrAll();
        if (codes == null) return;
        if (!codes.contains(companyCode)) {
            throw new BizException(ErrorCode.FORBIDDEN, "无该企业数据权限");
        }
    }
}
