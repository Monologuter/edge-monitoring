import { Client } from "@stomp/stompjs";
import { readTokens } from "@/auth/token";
import { wsBaseUrl } from "@/api/http";

type Subscription = { unsubscribe: () => void };

export function connectStomp(onError?: (e: unknown) => void) {
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
    subscribeJson: <T>(dest: string, cb: (data: T) => void): Subscription => {
      const sub = client.subscribe(dest, (msg) => {
        try {
          cb(JSON.parse(msg.body) as T);
        } catch {
          // ignore
        }
      });
      return { unsubscribe: () => sub.unsubscribe() };
    }
  };
}
