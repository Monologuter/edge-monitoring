package com.safetyfire.monitor.service;

import cn.hutool.core.io.FileUtil;
import cn.hutool.core.util.StrUtil;
import com.safetyfire.monitor.common.BizException;
import com.safetyfire.monitor.common.ErrorCode;
import com.safetyfire.monitor.domain.entity.FileObjectEntity;
import com.safetyfire.monitor.domain.vo.FileVO;
import com.safetyfire.monitor.mapper.FileObjectMapper;
import com.safetyfire.monitor.security.AuthUser;
import com.safetyfire.monitor.security.AuthUserHolder;
import com.safetyfire.monitor.util.CryptoUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.io.IOException;

/**
 * 文件服务：本地存储版本（可扩展 OSS）。
 */
@Service
public class FileService {
    private final FileObjectMapper fileObjectMapper;
    private final String localDir;

    public FileService(FileObjectMapper fileObjectMapper, @Value("${app.file.local-dir}") String localDir) {
        this.fileObjectMapper = fileObjectMapper;
        this.localDir = localDir;
    }

    @Transactional
    public FileVO upload(String bizType, MultipartFile file) {
        if (file == null || file.isEmpty()) {
            throw new BizException(ErrorCode.PARAM_INVALID, "文件不能为空");
        }
        String ct = StrUtil.blankToDefault(file.getContentType(), "application/octet-stream");
        if (file.getSize() > 20 * 1024 * 1024L) {
            throw new BizException(ErrorCode.PARAM_INVALID, "文件过大（最大 20MB）");
        }

        byte[] bytes;
        try {
            bytes = file.getBytes();
        } catch (IOException e) {
            throw new BizException(ErrorCode.SYSTEM_ERROR, "读取文件失败");
        }
        String sha = CryptoUtils.sha256Hex(bytes);

        FileObjectEntity exists = fileObjectMapper.findBySha256(sha);
        if (exists != null) {
            return toVo(exists);
        }

        String safeBiz = (bizType == null || bizType.isBlank()) ? "other" : bizType.trim();
        File dir = new File(localDir, safeBiz);
        FileUtil.mkdir(dir);

        String original = file.getOriginalFilename() == null ? "file" : file.getOriginalFilename();
        String ext = FileUtil.extName(original);
        String filename = sha + (ext.isBlank() ? "" : ("." + ext));
        File target = new File(dir, filename);
        FileUtil.writeBytes(bytes, target);

        AuthUser user = AuthUserHolder.get();

        FileObjectEntity e = new FileObjectEntity();
        e.setBizType(safeBiz);
        e.setOriginalName(original);
        e.setContentType(ct);
        e.setSizeBytes(file.getSize());
        e.setSha256(sha);
        e.setStorageType("LOCAL");
        e.setStoragePath(target.getAbsolutePath());
        e.setCreatedBy(user == null ? null : user.username());
        fileObjectMapper.insert(e);

        return toVo(e);
    }

    public FileObjectEntity get(Long id) {
        FileObjectEntity e = fileObjectMapper.findById(id);
        if (e == null) throw new BizException(ErrorCode.NOT_FOUND, "文件不存在");
        return e;
    }

    private FileVO toVo(FileObjectEntity e) {
        return new FileVO(e.getId(), e.getOriginalName(), e.getContentType(), e.getSizeBytes(), "/api/v1/files/" + e.getId());
    }
}

