const LS_KEY = "sf.tokens";
export function readTokens() {
    try {
        const raw = localStorage.getItem(LS_KEY);
        if (!raw)
            return null;
        const obj = JSON.parse(raw);
        if (!obj?.accessToken || !obj?.refreshToken)
            return null;
        return obj;
    }
    catch {
        return null;
    }
}
export function writeTokens(tokens) {
    localStorage.setItem(LS_KEY, JSON.stringify(tokens));
}
export function clearTokens() {
    localStorage.removeItem(LS_KEY);
}
