package com.safetyfire.monitor.domain.vo;

/**
 * AI盒子登录应答：兼容文档示例中的 token 字段（可选）。
 */
public class AiBoxLoginResponse extends AiBoxSimpleResponse {
    private String token;

    public AiBoxLoginResponse() {
    }

    public AiBoxLoginResponse(String code, String message, String token) {
        super(code, message);
        this.token = token;
    }

    public static AiBoxLoginResponse ok(String token) {
        return new AiBoxLoginResponse("200", "success", token);
    }

    public String getToken() {
        return token;
    }

    public void setToken(String token) {
        this.token = token;
    }
}

