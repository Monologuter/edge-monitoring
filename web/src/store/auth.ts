import { defineStore } from "pinia";
import { http } from "@/api/http";
import { clearTokens, readTokens, TokenPair, writeTokens } from "@/auth/token";

type MenuItem = {
  id: number;
  parentId: number | null;
  menuKey: string;
  menuName: string;
  path: string;
  icon: string | null;
  sortNo: number;
  children?: MenuItem[];
};

type UserVO = {
  id: number;
  username: string;
  displayName: string;
  roles: string[];
  permissions: string[];
  companyCodes: string[];
  menus: MenuItem[];
};

export const useAuthStore = defineStore("auth", {
  state: () => ({
    accessToken: "" as string,
    refreshToken: "" as string,
    me: null as UserVO | null,
    meLoaded: false as boolean
  }),
  getters: {
    isLoggedIn: (s) => Boolean(s.accessToken && s.refreshToken)
  },
  actions: {
    hydrate() {
      const obj = readTokens();
      if (!obj) return;
      this.accessToken = obj.accessToken || "";
      this.refreshToken = obj.refreshToken || "";
    },
    persist(tokens: TokenPair) {
      this.accessToken = tokens.accessToken;
      this.refreshToken = tokens.refreshToken;
      writeTokens(tokens);
    },
    async login(username: string, password: string) {
      const data = await http.post<TokenPair>("/api/v1/auth/login", { username, password });
      this.persist(data);
      this.meLoaded = false;
    },
    async fetchMe() {
      const data = await http.get<UserVO>("/api/v1/auth/me");
      this.me = data;
      this.meLoaded = true;
    },
    async refreshIfNeeded() {
      if (!this.refreshToken) throw new Error("no refresh token");
      const data = await http.post<TokenPair>("/api/v1/auth/refresh", { refreshToken: this.refreshToken }, { skipAuth: true });
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
