/* global up */
up.compiler(".diceroll", el => {
  let btn = el.querySelector("a.button");
  let output = el.querySelector("output");
  // let imgOutput = el.querySelector("img");

  btn.addEventListener("click", event => {
     let rollResult = Math.floor(Math.random() * 6) + 1;
     output.textContent = rollResult;
     // imgOutput.src = "/static/img/diceroll/dice" + rollResult + ".png";
     event.preventDefault();
  }, false);
});
