/* global up */
up.compiler(".slider", el => {
  let input = el.querySelector("input");
  let value = el.querySelector("output");

  value.textContent = input.value;
  input.addEventListener("input", event => {
    value.textContent = event.target.value;
  });
});
