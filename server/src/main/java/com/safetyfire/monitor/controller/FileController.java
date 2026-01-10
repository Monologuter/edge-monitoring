package com.safetyfire.monitor.controller;

import com.safetyfire.monitor.common.ApiResponse;
import com.safetyfire.monitor.config.Audit;
import com.safetyfire.monitor.domain.entity.FileObjectEntity;
import com.safetyfire.monitor.domain.vo.FileVO;
import com.safetyfire.monitor.service.FileService;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.io.FileInputStream;

/**
 * 文件接口：上传与下载。
 */
@RestController
@RequestMapping("/api/v1/files")
public class FileController {
    private final FileService fileService;

    public FileController(FileService fileService) {
        this.fileService = fileService;
    }

    @PostMapping("/upload")
    @PreAuthorize("hasAuthority('company:manage') or hasAuthority('alarm:manage') or hasAuthority('person:manage') or hasAuthority('car:manage')")
    @Audit(action = "file.upload")
    public ApiResponse<FileVO> upload(@RequestParam(required = false) String bizType, @RequestPart("file") MultipartFile file) {
        return ApiResponse.ok(fileService.upload(bizType, file));
    }

    @GetMapping("/{id}")
    @PreAuthorize("isAuthenticated()")
    public void download(@PathVariable Long id, HttpServletResponse response) throws Exception {
        FileObjectEntity e = fileService.get(id);
        response.setContentType(e.getContentType());
        response.setHeader("Content-Disposition", "inline; filename=\"" + e.getOriginalName() + "\"");
        File f = new File(e.getStoragePath());
        try (FileInputStream in = new FileInputStream(f)) {
            in.transferTo(response.getOutputStream());
        }
    }
}

