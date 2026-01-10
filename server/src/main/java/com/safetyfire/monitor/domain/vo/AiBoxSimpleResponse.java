package com.safetyfire.monitor.domain.vo;

/**
 * 兼容厂商 BOX 对接文档的返回格式（code/message 以字符串形式返回）。
 */
public class AiBoxSimpleResponse {
    private String code;
    private String message;

    public AiBoxSimpleResponse() {
    }

    public AiBoxSimpleResponse(String code, String message) {
        this.code = code;
        this.message = message;
    }

    public static AiBoxSimpleResponse ok() {
        return new AiBoxSimpleResponse("200", "success");
    }

    public static AiBoxSimpleResponse fail(String message) {
        return new AiBoxSimpleResponse("500", message == null ? "error" : message);
    }

    public String getCode() {
        return code;
    }

    public void setCode(String code) {
        this.code = code;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }
}

