import "../css/screen.scss";
import "./components";
import * as push from "./push";
import "./sentry";
import "./unpoly";

(async function () {
  await push.register();
  const subscription = await push.subscribe((window as any).VAPID_PUBLIC_KEY);
  await push.save(subscription);
})();
