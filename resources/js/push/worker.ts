/// <reference lib="webworker" />

declare const self: ServiceWorkerGlobalScope;
export {};

self.addEventListener("push", (event) => {
  const data = event.data?.json() ?? {};
  const title = data.title ?? "Notification";
  const options: NotificationOptions = {
    body: data.body,
    icon: data.icon,
    badge: data.badge,
    data: data.data,
    tag: data.tag,
    requireInteraction: data.requireInteraction ?? false,
  };
  event.waitUntil(self.registration.showNotification(title, options));
});

self.addEventListener("notificationclick", (event) => {
  event.notification.close();
  const url = event.notification.data?.url;
  if (!url) return;
  event.waitUntil(self.clients.openWindow(url));
});
