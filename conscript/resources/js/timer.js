(() => {
  const thisEl = document.currentScript;
  let n = thisEl.dataset.timerN * 1;
  const targetEl = document.querySelector("#timer-target");
  targetEl.innerText = `${n}`;
  schedule();
  return;

  function moveOn(btn) {
    if ( btn !== null ) {
      btn.addEventListener('click', event => {
        n = -10;
      });
    }
  }

  function schedule() {
    let submitBtn = document.querySelector("button.next-button[type='submit']");
    let nextBtn = document.querySelector("a.button.next-button");
    moveOn(submitBtn);
    moveOn(nextBtn);

    if (n >= 0) {
      targetEl.innerText = `${n--}`;
      setTimeout(schedule, 1000);
    } else if ( n < -5 ) {
      console.log("Moved on to another page already.");
    } else {
      if ( submitBtn !== null ) {
        submitBtn.click();
        console.log("bla1");
      } else if ( nextBtn !== null ) {
        nextBtn.click();
        console.log("bla2");
      } else {
        console.log("Timer ended, but no submit button or next button found. Doing nothing.");
      }
    }
  }
})();
