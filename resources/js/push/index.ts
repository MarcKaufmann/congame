export interface PushSubscriptionData {
  endpoint: string;
  keys: {
    p256dh: string;
    auth: string;
  };
}

let registration: ServiceWorkerRegistration | null = null;

export async function register(): Promise<ServiceWorkerRegistration> {
  if (!("serviceWorker" in navigator)) {
    throw new Error("Service workers not supported");
  }
  if (!("PushManager" in window)) {
    throw new Error("Push notifications not supported");
  }
  registration = await navigator.serviceWorker.register("/push-worker.js");
  await waitUntilActive(registration);
  return registration;
}

function waitUntilActive(reg: ServiceWorkerRegistration): Promise<void> {
  const sw = reg.installing ?? reg.waiting ?? reg.active;
  if (!sw) {
    return Promise.reject(new Error("No service worker found"));
  }
  if (sw.state === "activated") {
    return Promise.resolve();
  }
  return new Promise((resolve, reject) => {
    sw.addEventListener("statechange", function handler() {
      if (sw.state === "activated") {
        sw.removeEventListener("statechange", handler);
        resolve();
      } else if (sw.state === "redundant") {
        sw.removeEventListener("statechange", handler);
        reject(new Error("Service worker became redundant"));
      }
    });
  });
}

export async function getSubscription(): Promise<PushSubscription | null> {
  if (!registration) {
    throw new Error("Service worker not registered");
  }
  return registration.pushManager.getSubscription();
}

export async function subscribe(
  vapidPublicKey: string,
): Promise<PushSubscriptionData> {
  if (!registration) {
    throw new Error("Service worker not registered");
  }
  const subscription = await registration.pushManager.subscribe({
    userVisibleOnly: true,
    applicationServerKey: urlBase64ToUint8Array(vapidPublicKey),
  });
  const data = subscription.toJSON();
  return {
    endpoint: subscription.endpoint,
    keys: {
      p256dh: data.keys!.p256dh,
      auth: data.keys!.auth,
    },
  };
}

export async function unsubscribe(): Promise<boolean> {
  const subscription = await getSubscription();
  if (!subscription) return false;
  return subscription.unsubscribe();
}

export async function save(data: PushSubscriptionData) {
  return await fetch("/me/push", {
    credentials: "include",
    body: JSON.stringify(data),
    method: "PATCH",
    headers: {
      "content-type": "application/json",
    },
  });
}

function urlBase64ToUint8Array(base64String: string): Uint8Array {
  const padding = "=".repeat((4 - (base64String.length % 4)) % 4);
  const base64 = (base64String + padding).replace(/-/g, "+").replace(/_/g, "/");
  const rawData = atob(base64);
  const outputArray = new Uint8Array(rawData.length);
  for (let i = 0; i < rawData.length; ++i) {
    outputArray[i] = rawData.charCodeAt(i);
  }
  return outputArray;
}
