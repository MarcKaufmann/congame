/* global up */
if (!window.timerTargetCompilerDeclared) {
  window.timerTargetCompilerDeclared = true;
  up.compiler(
    "#timer-target",
    ((scriptEl) => (targetEl) => {
      const formEl = document.querySelector("form");
      const nextBtn = document.querySelector("a.button.next-button");
      let n = scriptEl.dataset.timerN * 1;
      targetEl.innerText = format(n);
      const handle = setInterval(schedule, 1000);
      return () => {
        clearInterval(handle);
      };

      function schedule() {
        if (n >= 0) {
          targetEl.innerText = format(n--);
        } else if (formEl) {
          formEl.submit();
        } else if (nextBtn) {
          nextBtn.click();
        } else {
          console.log(
            "Timer ended, but no submit button or next button found. Doing nothing.",
          );
        }
      }

      function format(seconds) {
        if (seconds > 60) {
          return `${Math.trunc(seconds / 60)} minutes and ${seconds % 60} seconds`;
        }
        return `${seconds} seconds`;
      }
    })(document.currentScript),
  );
}
