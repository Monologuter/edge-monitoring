import axios, { AxiosError, AxiosInstance } from "axios";
import { clearTokens, readTokens, TokenPair, writeTokens } from "@/auth/token";

type ApiResponse<T> = {
  code: number;
  message: string;
  data: T;
  requestId: string;
  timestamp: number;
};

type RequestOptions = {
  skipAuth?: boolean;
};

export const API_BASE_URL = import.meta.env.VITE_API_BASE || "http://localhost:9000";

export function wsBaseUrl() {
  return API_BASE_URL.replace(/^http:/, "ws:").replace(/^https:/, "wss:");
}

export function fileUrl(id: number) {
  return `${API_BASE_URL}/api/v1/files/${id}`;
}

let refreshPromise: Promise<TokenPair> | null = null;

async function refreshTokens(): Promise<TokenPair> {
  if (refreshPromise) return refreshPromise;
  const current = readTokens();
  if (!current?.refreshToken) throw new Error("no refresh token");

  refreshPromise = axios
    .post<ApiResponse<TokenPair>>(
      `${API_BASE_URL}/api/v1/auth/refresh`,
      { refreshToken: current.refreshToken },
      { timeout: 15000 }
    )
    .then((resp) => {
      if (resp.data.code !== 0) throw new Error(resp.data.message || "refresh failed");
      writeTokens(resp.data.data);
      return resp.data.data;
    })
    .finally(() => {
      refreshPromise = null;
    });

  return refreshPromise;
}

function createHttp(): AxiosInstance {
  const instance = axios.create({
    baseURL: API_BASE_URL,
    timeout: 15000
  });

  instance.interceptors.request.use((config) => {
    const opt = (config as any).options as RequestOptions | undefined;
    const tokens = readTokens();
    if (!opt?.skipAuth && tokens?.accessToken) {
      config.headers = config.headers || {};
      config.headers.Authorization = `Bearer ${tokens.accessToken}`;
    }
    return config;
  });

  instance.interceptors.response.use(
    async (resp) => {
      const body = resp.data as ApiResponse<any>;
      if (!body || typeof body.code !== "number") return resp.data;
      if (body.code === 0) return body.data;

      if (body.code === 2001 || body.code === 2002) {
        try {
          const newTokens = await refreshTokens();
          const original = resp.config;
          original.headers = original.headers || {};
          original.headers.Authorization = `Bearer ${newTokens.accessToken}`;
          return instance.request(original);
        } catch {
          clearTokens();
          void (async () => {
            try {
              const mod = await import("@/router");
              await mod.default.replace("/login");
            } catch {
              window.location.href = "/login";
            }
          })();
        }
      }

      throw new Error(body.message || "请求失败");
    },
    (err: AxiosError) => {
      throw err;
    }
  );

  return instance;
}

export const rawHttp = createHttp();

export const http = {
  get: <T>(url: string, options?: RequestOptions) => rawHttp.get<any, T>(url, { ...(options ? { options } : {}) } as any),
  post: <T>(url: string, data?: any, options?: RequestOptions) =>
    rawHttp.post<any, T>(url, data ?? {}, { ...(options ? { options } : {}) } as any),
  put: <T>(url: string, data?: any, options?: RequestOptions) =>
    rawHttp.put<any, T>(url, data ?? {}, { ...(options ? { options } : {}) } as any),
  del: <T>(url: string, options?: RequestOptions) =>
    rawHttp.delete<any, T>(url, { ...(options ? { options } : {}) } as any)
};

export async function uploadFile(bizType: string, file: File) {
  const fd = new FormData();
  fd.append("bizType", bizType);
  fd.append("file", file);
  return rawHttp.post<any, any>("/api/v1/files/upload", fd, {
    headers: { "Content-Type": "multipart/form-data" }
  });
}
