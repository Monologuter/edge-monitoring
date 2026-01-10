package com.safetyfire.monitor.service;

import com.safetyfire.monitor.common.BizException;
import com.safetyfire.monitor.common.ErrorCode;
import com.safetyfire.monitor.config.CachedBodyFilter;
import com.safetyfire.monitor.domain.entity.DeviceApiKeyEntity;
import com.safetyfire.monitor.mapper.DeviceApiKeyMapper;
import com.safetyfire.monitor.util.CryptoUtils;
import com.safetyfire.monitor.util.IpUtils;
import jakarta.servlet.http.HttpServletRequest;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;

import java.time.Duration;

/**
 * 设备上报鉴权：基于 API Key 的简单方案（后续可扩展为签名/HMAC/证书）。
 */
@Service
public class DeviceAuthService {
    public static final String HEADER = "X-Device-Api-Key";
    public static final String HEADER_TS = "X-Timestamp";
    public static final String HEADER_NONCE = "X-Nonce";
    public static final String HEADER_SIG = "X-Signature";

    private final DeviceApiKeyMapper deviceApiKeyMapper;
    private final StringRedisTemplate redisTemplate;

    public DeviceAuthService(DeviceApiKeyMapper deviceApiKeyMapper, StringRedisTemplate redisTemplate) {
        this.deviceApiKeyMapper = deviceApiKeyMapper;
        this.redisTemplate = redisTemplate;
    }

    public void assertValid(HttpServletRequest request, String apiKey, String timestamp, String nonce, String signature) {
        if (apiKey == null || apiKey.isBlank()) {
            throw new BizException(ErrorCode.DEVICE_AUTH_FAILED, "设备鉴权失败");
        }
        DeviceApiKeyEntity key = deviceApiKeyMapper.findByApiKey(apiKey.trim());
        if (key == null || key.getEnabled() == null || key.getEnabled() != 1) {
            throw new BizException(ErrorCode.DEVICE_AUTH_FAILED, "设备鉴权失败");
        }

        // IP 白名单
        String ip = IpUtils.getClientIp(request);
        if (key.getAllowedIps() != null && !key.getAllowedIps().isBlank()) {
            boolean ok = false;
            for (String allowed : key.getAllowedIps().split(",")) {
                if (allowed != null && !allowed.isBlank() && allowed.trim().equals(ip)) {
                    ok = true;
                    break;
                }
            }
            if (!ok) {
                throw new BizException(ErrorCode.DEVICE_AUTH_FAILED, "设备鉴权失败");
            }
        }

        // 限流：按 apiKey + ip 每分钟限频
        int limit = key.getRateLimitPerMinute() == null ? 120 : key.getRateLimitPerMinute();
        String rlKey = "rl:ingest:" + key.getApiKey() + ":" + ip;
        Long c = redisTemplate.opsForValue().increment(rlKey);
        if (c != null && c == 1L) {
            redisTemplate.expire(rlKey, Duration.ofSeconds(60));
        }
        if (c != null && c > limit) {
            throw new BizException(ErrorCode.RATE_LIMITED, "请求过于频繁，请稍后重试");
        }

        // 签名校验：timestamp + nonce + bodySha256
        long ts;
        try {
            ts = Long.parseLong(timestamp == null ? "" : timestamp.trim());
        } catch (Exception e) {
            throw new BizException(ErrorCode.DEVICE_AUTH_FAILED, "设备鉴权失败");
        }
        long now = System.currentTimeMillis();
        if (Math.abs(now - ts) > 5 * 60_000L) {
            throw new BizException(ErrorCode.DEVICE_AUTH_FAILED, "设备鉴权失败");
        }
        if (nonce == null || nonce.isBlank() || signature == null || signature.isBlank()) {
            throw new BizException(ErrorCode.DEVICE_AUTH_FAILED, "设备鉴权失败");
        }

        String nonceKey = "nonce:" + key.getApiKey() + ":" + nonce.trim();
        Boolean first = redisTemplate.opsForValue().setIfAbsent(nonceKey, "1", Duration.ofMinutes(10));
        if (first == null || !first) {
            throw new BizException(ErrorCode.DEVICE_AUTH_FAILED, "设备鉴权失败");
        }

        byte[] body = (byte[]) request.getAttribute(CachedBodyFilter.ATTR_BODY);
        String bodySha = CryptoUtils.sha256Hex(body == null ? new byte[0] : body);
        String canonical = canonical(key.getApiKey(), ts, nonce.trim(), bodySha);
        String expected = CryptoUtils.hmacSha256Hex(key.getApiSecret() == null ? "" : key.getApiSecret(), canonical);
        if (!expected.equalsIgnoreCase(signature.trim())) {
            throw new BizException(ErrorCode.DEVICE_AUTH_FAILED, "设备鉴权失败");
        }
    }

    /**
     * MQTT 接入鉴权：
     * - 不具备 HTTP 的真实 IP（由 broker 管理网络层），因此不做 IP 白名单校验；
     * - 仍保留 nonce 防重放与限流（按 apiKey 每分钟）。
     *
     * @param dataBytes 业务 data 的 UTF-8 字节（稳定字符串，便于签名）
     */
    public void assertValidMqtt(String apiKey, long timestamp, String nonce, String signature, byte[] dataBytes) {
        if (apiKey == null || apiKey.isBlank()) {
            throw new BizException(ErrorCode.DEVICE_AUTH_FAILED, "设备鉴权失败");
        }
        DeviceApiKeyEntity key = deviceApiKeyMapper.findByApiKey(apiKey.trim());
        if (key == null || key.getEnabled() == null || key.getEnabled() != 1) {
            throw new BizException(ErrorCode.DEVICE_AUTH_FAILED, "设备鉴权失败");
        }

        long now = System.currentTimeMillis();
        if (Math.abs(now - timestamp) > 5 * 60_000L) {
            throw new BizException(ErrorCode.DEVICE_AUTH_FAILED, "设备鉴权失败");
        }
        if (nonce == null || nonce.isBlank() || signature == null || signature.isBlank()) {
            throw new BizException(ErrorCode.DEVICE_AUTH_FAILED, "设备鉴权失败");
        }

        // 限流：按 apiKey 每分钟限频（MQTT 无 IP）
        int limit = key.getRateLimitPerMinute() == null ? 120 : key.getRateLimitPerMinute();
        String rlKey = "rl:mqtt:" + key.getApiKey();
        Long c = redisTemplate.opsForValue().increment(rlKey);
        if (c != null && c == 1L) {
            redisTemplate.expire(rlKey, Duration.ofSeconds(60));
        }
        if (c != null && c > limit) {
            throw new BizException(ErrorCode.RATE_LIMITED, "请求过于频繁，请稍后重试");
        }

        String nonceKey = "nonce:mqtt:" + key.getApiKey() + ":" + nonce.trim();
        Boolean first = redisTemplate.opsForValue().setIfAbsent(nonceKey, "1", Duration.ofMinutes(10));
        if (first == null || !first) {
            throw new BizException(ErrorCode.DEVICE_AUTH_FAILED, "设备鉴权失败");
        }

        String bodySha = CryptoUtils.sha256Hex(dataBytes == null ? new byte[0] : dataBytes);
        String canonical = canonical(key.getApiKey(), timestamp, nonce.trim(), bodySha);
        String expected = CryptoUtils.hmacSha256Hex(key.getApiSecret() == null ? "" : key.getApiSecret(), canonical);
        if (!expected.equalsIgnoreCase(signature.trim())) {
            throw new BizException(ErrorCode.DEVICE_AUTH_FAILED, "设备鉴权失败");
        }
    }

    private static String canonical(String apiKey, long ts, String nonce, String bodySha) {
        return apiKey + "\n" + ts + "\n" + nonce + "\n" + bodySha;
    }
}
