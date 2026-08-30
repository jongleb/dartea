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
  while (true) {
    if (i > n$1) {
      return acc;
    } else {
      if (acc === "") {
        const $s3 = i + 1;
        const $s4 = $$String.fromInt(fib(i));
        i = $s3;
        acc = $s4;
        continue;
      } else {
        const $s1 = i + 1;
        const $s2 = acc + (" " + $$String.fromInt(fib(i)));
        i = $s1;
        acc = $s2;
        continue;
      }
    }
  }
};
  return go(0, "");
};
const main = fibUpTo(15) + (" | fib 20 = " + $$String.fromInt(fib(20)));
export { fib, fibUpTo, main };
