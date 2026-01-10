package com.safetyfire.monitor.service;

import cn.hutool.core.io.FileUtil;
import cn.hutool.core.util.StrUtil;
import com.safetyfire.monitor.common.BizException;
import com.safetyfire.monitor.common.ErrorCode;
import com.safetyfire.monitor.domain.entity.FileObjectEntity;
import com.safetyfire.monitor.mapper.FileObjectMapper;
import com.safetyfire.monitor.security.AuthUser;
import com.safetyfire.monitor.security.AuthUserHolder;
import com.safetyfire.monitor.util.CryptoUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.io.File;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

/**
 * AI盒子图片/录像存储：
 * - 将二进制写入“对外文件服务目录”（如 /opt/nginxTemp/html），并生成 publicUrl（如 http://47.120.48.64:9004/）。
 * - 同时写入 file_object，便于系统内下载与追溯。
 */
@Service
public class AiBoxMediaStorageService {
    private static final DateTimeFormatter DAY = DateTimeFormatter.ofPattern("yyyyMMdd");

    private final FileObjectMapper fileObjectMapper;
    private final String fileRoot;
    private final String baseUrl;

    public AiBoxMediaStorageService(FileObjectMapper fileObjectMapper,
                                    @Value("${app.ai-box.file-root:/opt/nginxTemp/html}") String fileRoot,
                                    @Value("${app.ai-box.base-url:http://47.120.48.64:9004/}") String baseUrl) {
        this.fileObjectMapper = fileObjectMapper;
        this.fileRoot = fileRoot;
        this.baseUrl = baseUrl;
    }

    public StoredFile store(String bizType, String originalName, String contentType, byte[] bytes) {
        if (bytes == null || bytes.length == 0) {
            throw new BizException(ErrorCode.PARAM_INVALID, "文件内容为空");
        }
        String sha = CryptoUtils.sha256Hex(bytes);

        FileObjectEntity exists = fileObjectMapper.findBySha256(sha);
        if (exists != null) {
            return new StoredFile(exists.getId(), sha, exists.getStoragePath(), publicUrlByPath(exists.getStoragePath()));
        }

        String day = LocalDate.now().format(DAY);
        String safeBiz = StrUtil.blankToDefault(bizType, "ai_box");
        String ext = FileUtil.extName(StrUtil.blankToDefault(originalName, "file"));
        String filename = sha + (ext.isBlank() ? "" : ("." + ext));
        String relative = "box/" + safeBiz + "/" + day + "/" + filename;

        File rootDir = ensureWritableRoot();
        File target = new File(rootDir, relative);
        FileUtil.mkdir(target.getParentFile());
        FileUtil.writeBytes(bytes, target);

        AuthUser user = AuthUserHolder.get();

        FileObjectEntity e = new FileObjectEntity();
        e.setBizType(safeBiz);
        e.setOriginalName(StrUtil.blankToDefault(originalName, filename));
        e.setContentType(StrUtil.blankToDefault(contentType, "application/octet-stream"));
        e.setSizeBytes((long) bytes.length);
        e.setSha256(sha);
        e.setStorageType("AI_BOX_PUBLIC");
        e.setStoragePath(target.getAbsolutePath());
        e.setCreatedBy(user == null ? null : user.username());
        fileObjectMapper.insert(e);

        return new StoredFile(e.getId(), sha, target.getAbsolutePath(), joinUrl(baseUrl, relative));
    }

    private File ensureWritableRoot() {
        // 生产通常会指向 nginx/ftp 文件服务目录；开发环境若无权限，则降级到项目目录下
        File root = new File(fileRoot);
        if (!root.exists()) {
            try {
                FileUtil.mkdir(root);
            } catch (Exception ignore) {
            }
        }
        if (root.exists() && root.isDirectory() && root.canWrite()) return root;
        File fallback = new File("./.data/ai-box-public");
        FileUtil.mkdir(fallback);
        return fallback;
    }

    private String publicUrlByPath(String storagePath) {
        if (storagePath == null) return null;
        File root = new File(fileRoot);
        try {
            String rp = FileUtil.subPath(root.getAbsolutePath(), storagePath);
            return joinUrl(baseUrl, rp);
        } catch (Exception ignore) {
        }
        // 无法推断相对路径，则返回文件下载接口
        return null;
    }

    private static String joinUrl(String base, String relative) {
        if (base == null) return null;
        String b = base.endsWith("/") ? base : (base + "/");
        String r = relative == null ? "" : relative;
        while (r.startsWith("/")) r = r.substring(1);
        return b + r;
    }

    public record StoredFile(Long fileObjectId, String sha256, String storagePath, String publicUrl) {
    }
}

