(() => {
  const thisEl = document.currentScript;
  let n = thisEl.dataset.timerN * 1;
  const targetEl = document.querySelector("#timer-target");
  targetEl.innerText = `${n}`;
  schedule();
  return;

  function schedule() {
    if (n >= 0) {
      targetEl.innerText = `${n--}`;
      setTimeout(schedule, 1000);
    }
  }
})();
