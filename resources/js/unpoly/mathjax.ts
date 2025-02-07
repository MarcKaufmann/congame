import up from "./unpoly";

up.on("up:fragment:inserted", (event: Event) => {
  const mathjax = (window as any).MathJax;
  if (mathjax !== undefined) {
    mathjax.typeset();
  }
});
