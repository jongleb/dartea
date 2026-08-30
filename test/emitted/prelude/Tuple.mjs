import * as Dartea_runtime from "./Dartea_runtime.mjs";
const pair = (a, b) => [a, b];
const first = t => {
  const x = t[0];
  const y = t[1];
  return x;
};
const second = t$1 => {
  const x$1 = t$1[0];
  const y$1 = t$1[1];
  return y$1;
};
const mapFirst = (func, $p1) => {
  const x$2 = $p1[0];
  const y$2 = $p1[1];
  return [Dartea_runtime.$$apply1(func, x$2), y$2];
};
const mapSecond = (func$1, $p1$1) => {
  const x$3 = $p1$1[0];
  const y$3 = $p1$1[1];
  return [x$3, Dartea_runtime.$$apply1(func$1, y$3)];
};
const mapBoth = (funcA, funcB, $p2) => {
  const x$4 = $p2[0];
  const y$4 = $p2[1];
  return [Dartea_runtime.$$apply1(funcA, x$4), Dartea_runtime.$$apply1(funcB, y$4)];
};
export { first, mapBoth, mapFirst, mapSecond, pair, second };
