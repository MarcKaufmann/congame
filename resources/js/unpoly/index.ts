import up from "./unpoly";

import "./calibration";
import "./counter";
import "./filesize";
import "./mask";
import "./toggleable";
import "./track-timings";

for (const selector of ["cg-radios-with-other"]) {
  up.form.config.fieldSelectors.push(selector);
}
