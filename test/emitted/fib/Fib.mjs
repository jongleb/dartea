import * as $$String from "./String.mjs";
const fib = n => {
  switch (n) {
    case 0:
      return 0;
    case 1:
      return 1;
    default:
      return fib(n - 1) + fib(n - 2);
  }
};
const fibUpTo = n$1 => {
  const go = (i, acc) => {
  if (i > n$1) {
    return acc;
  } else {
    if (acc === "") {
      return go(i + 1, $$String.fromInt(fib(i)));
    } else {
      return go(i + 1, acc + (" " + $$String.fromInt(fib(i))));
    }
  }
};
  return go(0, "");
};
const main = fibUpTo(15) + (" | fib 20 = " + $$String.fromInt(fib(20)));
export { fib, fibUpTo, main };
