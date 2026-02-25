import up from "./unpoly";

import "./calibration";
import "./counter";
import "./filesize";
import "./mask";
import "./mathjax";
import "./push";
import "./toggleable";
import "./track-timings";

for (const selector of ["cg-radios-with-other"]) {
  up.form.config.fieldSelectors.push(selector);
}
