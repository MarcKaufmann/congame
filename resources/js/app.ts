import "../css/screen.scss";
import "./components";
import * as push from "./push";
import "./sentry";
import "./unpoly";

(async function () {
  await push.register();
  const subscription = await push.subscribe(
    "BHhjOB3QggiXZgA0J7JLdvAl8cnBLSn2-O59WxIDLQG8UDaNcyk8LurUcA1xJlk4fB2ZPVK769ZVu8ogekHJplc",
  );
  await push.save(subscription);
})();
