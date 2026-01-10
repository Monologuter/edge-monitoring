import { defineStore } from "pinia";
import { http } from "@/api/http";
import { clearTokens, readTokens, writeTokens } from "@/auth/token";
export const useAuthStore = defineStore("auth", {
    state: () => ({
        accessToken: "",
        refreshToken: "",
        me: null,
        meLoaded: false
    }),
    getters: {
        isLoggedIn: (s) => Boolean(s.accessToken && s.refreshToken)
    },
    actions: {
        hydrate() {
            const obj = readTokens();
            if (!obj)
                return;
            this.accessToken = obj.accessToken || "";
            this.refreshToken = obj.refreshToken || "";
        },
        persist(tokens) {
            this.accessToken = tokens.accessToken;
            this.refreshToken = tokens.refreshToken;
            writeTokens(tokens);
        },
        async login(username, password) {
            const data = await http.post("/api/v1/auth/login", { username, password });
            this.persist(data);
            this.meLoaded = false;
        },
        async fetchMe() {
            const data = await http.get("/api/v1/auth/me");
            this.me = data;
            this.meLoaded = true;
        },
        async refreshIfNeeded() {
            if (!this.refreshToken)
                throw new Error("no refresh token");
            const data = await http.post("/api/v1/auth/refresh", { refreshToken: this.refreshToken }, { skipAuth: true });
            this.persist(data);
        },
        logout() {
            this.accessToken = "";
            this.refreshToken = "";
            this.me = null;
            this.meLoaded = false;
            clearTokens();
        }
    }
});
