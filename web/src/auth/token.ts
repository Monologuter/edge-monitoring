export type TokenPair = {
  accessToken: string;
  refreshToken: string;
  expiresInSeconds: number;
};

const LS_KEY = "sf.tokens";

export function readTokens(): TokenPair | null {
  try {
    const raw = localStorage.getItem(LS_KEY);
    if (!raw) return null;
    const obj = JSON.parse(raw) as TokenPair;
    if (!obj?.accessToken || !obj?.refreshToken) return null;
    return obj;
  } catch {
    return null;
  }
}

export function writeTokens(tokens: TokenPair) {
  localStorage.setItem(LS_KEY, JSON.stringify(tokens));
}

export function clearTokens() {
  localStorage.removeItem(LS_KEY);
}

