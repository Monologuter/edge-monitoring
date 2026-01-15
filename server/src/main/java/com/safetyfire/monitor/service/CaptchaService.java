package com.safetyfire.monitor.service;

import com.safetyfire.monitor.config.AuthProperties;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;

import javax.imageio.ImageIO;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.time.Duration;
import java.util.Base64;
import java.util.UUID;

/**
 * 登录验证码服务。
 */
@Service
public class CaptchaService {
    private static final String PREFIX = "captcha:";

    private final AuthProperties props;
    private final StringRedisTemplate redisTemplate;

    public CaptchaService(AuthProperties props, StringRedisTemplate redisTemplate) {
        this.props = props;
        this.redisTemplate = redisTemplate;
    }

    public CaptchaResult generate() {
        String code = randomDigits(Math.max(props.getCaptchaLength(), 4));
        String id = UUID.randomUUID().toString().replace("-", "");
        redisTemplate.opsForValue().set(PREFIX + id, code, Duration.ofSeconds(props.getCaptchaTtlSeconds()));
        return new CaptchaResult(id, renderBase64(code));
    }

    public boolean validate(String id, String code) {
        if (id == null || id.isBlank() || code == null || code.isBlank()) {
            return false;
        }
        String key = PREFIX + id.trim();
        String stored = redisTemplate.opsForValue().get(key);
        if (stored == null) return false;
        redisTemplate.delete(key);
        return stored.equalsIgnoreCase(code.trim());
    }

    private String randomDigits(int len) {
        String digits = "0123456789";
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < len; i++) {
            sb.append(digits.charAt((int) (Math.random() * digits.length())));
        }
        return sb.toString();
    }

    private String renderBase64(String code) {
        int width = 120;
        int height = 40;
        BufferedImage image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
        Graphics2D g = image.createGraphics();
        g.setColor(new Color(244, 247, 255));
        g.fillRect(0, 0, width, height);
        g.setColor(new Color(91, 109, 255));
        g.setFont(new Font("SansSerif", Font.BOLD, 22));
        g.drawString(code, 18, 28);
        g.setColor(new Color(200, 210, 240));
        for (int i = 0; i < 6; i++) {
            g.drawLine(10 + i * 18, 8, 20 + i * 16, 32);
        }
        g.dispose();
        try (ByteArrayOutputStream baos = new ByteArrayOutputStream()) {
            ImageIO.write(image, "png", baos);
            String base64 = Base64.getEncoder().encodeToString(baos.toByteArray());
            return "data:image/png;base64," + base64;
        } catch (Exception e) {
            throw new IllegalStateException("生成验证码失败");
        }
    }

    public record CaptchaResult(String captchaId, String imageBase64) {
    }
}
