package com.safetyfire.monitor.domain.vo;

/**
 * AI盒子图片/录像上传应答：在 success 的基础上可返回可访问 URL（设备若忽略也不影响）。
 */
public class AiBoxUploadResponse extends AiBoxSimpleResponse {
    private String url;

    public AiBoxUploadResponse() {
    }

    public AiBoxUploadResponse(String code, String message, String url) {
        super(code, message);
        this.url = url;
    }

    public static AiBoxUploadResponse ok(String url) {
        return new AiBoxUploadResponse("200", "success", url);
    }

    public String getUrl() {
        return url;
    }

    public void setUrl(String url) {
        this.url = url;
    }
}

