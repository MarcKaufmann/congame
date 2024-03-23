(() => {
  const thisEl = document.currentScript;
  let n = thisEl.dataset.timerN * 1;
  const targetEl = document.querySelector("#timer-target");
  targetEl.innerText = `${n}`;
  schedule();
  return;

  function schedule() {
    let submitBtn = document.querySelector("button.next-button[type='submit']");
    let nextBtn = document.querySelector("a.button.next-button");
    if (n >= 0) {
      targetEl.innerText = `${n--}`;
      setTimeout(schedule, 1000);
    } else {
      if ( submitBtn !== null ) {
        submitBtn.click();
      } else if ( nextBtn !== null ) {
        nextBtn.click();
      } else {
        console.log("Timer ended, but no submit button or next button found. Doing nothing.");
      }
    }
  }
})();
