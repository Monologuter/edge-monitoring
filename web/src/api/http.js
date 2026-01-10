import axios from "axios";
import { clearTokens, readTokens, writeTokens } from "@/auth/token";
export const API_BASE_URL = import.meta.env.VITE_API_BASE || "http://localhost:9000";
export function wsBaseUrl() {
    return API_BASE_URL.replace(/^http:/, "ws:").replace(/^https:/, "wss:");
}
export function fileUrl(id) {
    return `${API_BASE_URL}/api/v1/files/${id}`;
}
let refreshPromise = null;
async function refreshTokens() {
    if (refreshPromise)
        return refreshPromise;
    const current = readTokens();
    if (!current?.refreshToken)
        throw new Error("no refresh token");
    refreshPromise = axios
        .post(`${API_BASE_URL}/api/v1/auth/refresh`, { refreshToken: current.refreshToken }, { timeout: 15000 })
        .then((resp) => {
        if (resp.data.code !== 0)
            throw new Error(resp.data.message || "refresh failed");
        writeTokens(resp.data.data);
        return resp.data.data;
    })
        .finally(() => {
        refreshPromise = null;
    });
    return refreshPromise;
}
function createHttp() {
    const instance = axios.create({
        baseURL: API_BASE_URL,
        timeout: 15000
    });
    instance.interceptors.request.use((config) => {
        const opt = config.options;
        const tokens = readTokens();
        if (!opt?.skipAuth && tokens?.accessToken) {
            config.headers = config.headers || {};
            config.headers.Authorization = `Bearer ${tokens.accessToken}`;
        }
        return config;
    });
    instance.interceptors.response.use(async (resp) => {
        const body = resp.data;
        if (!body || typeof body.code !== "number")
            return resp.data;
        if (body.code === 0)
            return body.data;
        if (body.code === 2001 || body.code === 2002) {
            try {
                const newTokens = await refreshTokens();
                const original = resp.config;
                original.headers = original.headers || {};
                original.headers.Authorization = `Bearer ${newTokens.accessToken}`;
                return instance.request(original);
            }
            catch {
                clearTokens();
                void (async () => {
                    try {
                        const mod = await import("@/router");
                        await mod.default.replace("/login");
                    }
                    catch {
                        window.location.href = "/login";
                    }
                })();
            }
        }
        throw new Error(body.message || "请求失败");
    }, (err) => {
        throw err;
    });
    return instance;
}
export const rawHttp = createHttp();
export const http = {
    get: (url, options) => rawHttp.get(url, { ...(options ? { options } : {}) }),
    post: (url, data, options) => rawHttp.post(url, data ?? {}, { ...(options ? { options } : {}) }),
    put: (url, data, options) => rawHttp.put(url, data ?? {}, { ...(options ? { options } : {}) }),
    del: (url, options) => rawHttp.delete(url, { ...(options ? { options } : {}) })
};
export async function uploadFile(bizType, file) {
    const fd = new FormData();
    fd.append("bizType", bizType);
    fd.append("file", file);
    return rawHttp.post("/api/v1/files/upload", fd, {
        headers: { "Content-Type": "multipart/form-data" }
    });
}
