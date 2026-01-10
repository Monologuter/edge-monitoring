import { Client } from "@stomp/stompjs";
import { readTokens } from "@/auth/token";
import { wsBaseUrl } from "@/api/http";
export function connectStomp(onError) {
    const tokens = readTokens();
    const client = new Client({
        brokerURL: `${wsBaseUrl()}/ws`,
        reconnectDelay: 2000,
        heartbeatIncoming: 10000,
        heartbeatOutgoing: 10000,
        connectHeaders: tokens?.accessToken ? { Authorization: `Bearer ${tokens.accessToken}` } : {}
    });
    client.onStompError = (frame) => {
        onError?.(frame.headers["message"] || frame.body);
    };
    client.onWebSocketError = (evt) => {
        onError?.(evt);
    };
    return {
        client,
        activate: () => client.activate(),
        deactivate: () => client.deactivate(),
        subscribeJson: (dest, cb) => {
            const sub = client.subscribe(dest, (msg) => {
                try {
                    cb(JSON.parse(msg.body));
                }
                catch {
                    // ignore
                }
            });
            return { unsubscribe: () => sub.unsubscribe() };
        }
    };
}
