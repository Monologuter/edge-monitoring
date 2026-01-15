package com.safetyfire.monitor.service;

import com.safetyfire.monitor.common.PageResponse;
import com.safetyfire.monitor.domain.dto.LinkageCreateRequest;
import com.safetyfire.monitor.domain.entity.LinkageEventEntity;
import com.safetyfire.monitor.mapper.LinkageEventMapper;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

/**
 * 联动服务（FR-11）：音柱/TTS 使用 HTTP 协议。
 */
@Service
public class LinkageService {
    private final LinkageEventMapper mapper;
    private final IpSpeakerService ipSpeakerService;

    public LinkageService(LinkageEventMapper mapper, IpSpeakerService ipSpeakerService) {
        this.mapper = mapper;
        this.ipSpeakerService = ipSpeakerService;
    }

    @Transactional
    public Long create(LinkageCreateRequest req) {
        LinkageEventEntity e = new LinkageEventEntity();
        e.setAlarmId(req.alarmId());
        e.setLinkageType(req.linkageType());
        e.setTarget(req.target());
        e.setPayload(req.payload());
        e.setStatus("PENDING");
        e.setLastError(null);
        mapper.insert(e);
        return e.getId();
    }

    public PageResponse<LinkageEventEntity> list(String status, int page, int pageSize) {
        int offset = (page - 1) * pageSize;
        List<LinkageEventEntity> list = mapper.list(offset, pageSize, status);
        long total = mapper.count(status);
        return new PageResponse<>(list, page, pageSize, total);
    }

    /**
     * 执行一条联动：
     * - IP_TTS/HTTP_TTS：payload 为 TTS 文本，target 可选（覆盖默认音柱地址）
     */
    public void execute(LinkageEventEntity e) throws Exception {
        if ("IP_TTS".equalsIgnoreCase(e.getLinkageType()) || "HTTP_TTS".equalsIgnoreCase(e.getLinkageType())) {
            ipSpeakerService.sendTts(e.getPayload(), e.getTarget());
            return;
        }
        // 其他类型暂留空：生产可扩展为 HTTP Webhook、短信、邮件等
        throw new IllegalArgumentException("不支持的联动类型：" + e.getLinkageType());
    }

    @Transactional
    public void markSent(Long id) {
        mapper.markSent(id);
    }

    @Transactional
    public void markFailed(Long id, String err) {
        mapper.markFailed(id, err);
    }

    public List<LinkageEventEntity> pickPending(int limit) {
        return mapper.listPending(limit);
    }
}
