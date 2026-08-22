// Derived from elm/core -- https://github.com/elm/core
// Copyright 2014-present Evan Czaplicki, BSD 3-Clause License.
// Emitted by dartea; its LICENSE file carries the full text.
import * as Dartea_runtime from "./Dartea_runtime.mjs";
const LT = "LT";
const EQ = "EQ";
const GT = "GT";
const comparisonOf = (a, b) => Dartea_runtime.$$cmp(a, b);
const compare = (x, y) => {
  const ordering = comparisonOf(x, y);
  if (ordering < 0) {
    return LT;
  } else {
    if (ordering === 0) {
      return EQ;
    } else {
      return GT;
    }
  }
};
const min = (x$1, y$1) => {
  if (Dartea_runtime.$$cmp(x$1, y$1) < 0) {
    return x$1;
  } else {
    return y$1;
  }
};
const max = (x$2, y$2) => {
  if (Dartea_runtime.$$cmp(x$2, y$2) > 0) {
    return x$2;
  } else {
    return y$2;
  }
};
const identity = x$3 => x$3;
const always = (a, b) => a;
const toFloat = x => x;
const round = x => Math.round(x);
const floor = x => Math.floor(x);
const ceiling = x => Math.ceil(x);
const truncate = x => x | 0;
const not = x => !x;
const xor = (a, b) => a !== b;
const modBy = (a, b) => Dartea_runtime.$$modBy(a, b);
const remainderBy = (a, b) => b % a;
const negate = n => 0 - n;
const abs = n$1 => {
  if (n$1 < 0) {
    return 0 - n$1;
  } else {
    return n$1;
  }
};
const clamp = (low, high, number) => {
  if (number < low) {
    return low;
  } else {
    if (number > high) {
      return high;
    } else {
      return number;
    }
  }
};
const sqrt = x => Math.sqrt(x);
const logBase = (base, number$1) => Math.log(number$1) / Math.log(base);
const e = Math.E;
const $$isNaN = x => isNaN(x);
const isInfinite = x => (x === Infinity) || (x === -Infinity);
const radians = angleInRadians => angleInRadians;
const pi = Math.PI;
const degrees = angleInDegrees => (angleInDegrees * pi) / 180;
const turns = angleInTurns => (2 * pi) * angleInTurns;
const cos = x => Math.cos(x);
const sin = x => Math.sin(x);
const tan = x => Math.tan(x);
const acos = x => Math.acos(x);
const asin = x => Math.asin(x);
const atan = x => Math.atan(x);
const atan2 = (a, b) => Math.atan2(a, b);
const composeL = (g, f, x$4) => Dartea_runtime.$$curry(g, [Dartea_runtime.$$curry(f, [x$4])]);
const composeR = (f$1, g$1, x$5) => Dartea_runtime.$$curry(g$1, [Dartea_runtime.$$curry(f$1, [x$5])]);
const apR = (x$6, f$2) => Dartea_runtime.$$curry(f$2, [x$6]);
const apL = (f$3, x$7) => Dartea_runtime.$$curry(f$3, [x$7]);
export { EQ, GT, LT, abs, acos, always, apL, apR, asin, atan, atan2, ceiling, clamp, compare, composeL, composeR, cos, degrees, e, floor, identity, isInfinite, $$isNaN, logBase, max, min, modBy, negate, not, pi, radians, remainderBy, round, sin, sqrt, tan, toFloat, truncate, turns, xor };
