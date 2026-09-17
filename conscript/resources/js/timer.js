/* global up */
if (!window.timerTargetCompilerDeclared) {
  window.timerTargetCompilerDeclared = true;
  up.compiler("#timer-target", (targetEl) => {
    const formEl = document.querySelector("form");
    const nextBtn = document.querySelector("a.button.next-button");
    console.log(targetEl);
    let n = targetEl.dataset.timerN * 1;
    targetEl.innerText = format(n);
    const handle = setInterval(schedule, 1000);
    return unschedule;

    function schedule() {
      if (n >= 0) {
        targetEl.innerText = format(n--);
        return;
      }
      if (formEl) {
        formEl.submit();
      } else if (nextBtn) {
        nextBtn.click();
      } else {
        console.log(
          "Timer ended, but no submit button or next button found. Doing nothing.",
        );
      }
      unschedule();
    }

    function unschedule() {
      clearInterval(handle);
    }

    function format(seconds) {
      if (seconds > 60) {
        return `${Math.trunc(seconds / 60)} minutes and ${seconds % 60} seconds`;
      }
      return `${seconds} seconds`;
    }
  });
}
