// Derived from elm/core -- https://github.com/elm/core
// Copyright 2014-present Evan Czaplicki, BSD 3-Clause License.
// Emitted by dartea; its LICENSE file carries the full text.
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
export { first, pair, second };
