import * as push from "../push";
import up from "./unpoly";

up.compiler("[data-request-push-permissions]", (el: HTMLInputElement) => {
  el.addEventListener("click", onClick);
  return () => {
    el.removeEventListener("click", onClick);
  };
  async function onClick() {
    await push.register();
    const subscription = await push.subscribe((window as any).VAPID_PUBLIC_KEY);
    await push.save(subscription);
  }
});
