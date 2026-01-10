package com.safetyfire.monitor.config;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.web.servlet.MultipartConfigFactory;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.util.unit.DataSize;

import jakarta.servlet.MultipartConfigElement;

/**
 * AI盒子上传（图片/录像）通常会比默认限制更大，这里提供一个可通过环境变量覆盖的默认值。
 */
@Configuration
public class AiBoxMultipartConfig {
    @Bean
    public MultipartConfigElement multipartConfigElement(
            @Value("${app.ai-box.multipart.max-file-size:200MB}") String maxFileSize,
            @Value("${app.ai-box.multipart.max-request-size:200MB}") String maxRequestSize
    ) {
        MultipartConfigFactory factory = new MultipartConfigFactory();
        factory.setMaxFileSize(DataSize.parse(maxFileSize));
        factory.setMaxRequestSize(DataSize.parse(maxRequestSize));
        return factory.createMultipartConfig();
    }
}

