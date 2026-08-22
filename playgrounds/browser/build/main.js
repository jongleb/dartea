const Dartea_runtime = (() => {
// Derived from elm/core -- https://github.com/elm/core
// Copyright 2014-present Evan Czaplicki, BSD 3-Clause License.
// Emitted by dartea; its LICENSE file carries the full text.
const $$curry = (f, args) => {
  const n = f.length === 0 ? 1 : f.length;
  if (args.length === n) return f(...args);
  if (args.length < n) return (...more) => $$curry(f, [...args, ...more]);
  return $$curry(f(...args.slice(0, n)), args.slice(n));
};

const $$append = (xs, ys) => {
  if (typeof xs === "string") return xs + ys;
  if (xs === 0) return ys;
  const root = { hd: xs.hd, tl: ys };
  let last = root;
  for (let rest = xs.tl; rest !== 0; rest = rest.tl) {
    last = last.tl = { hd: rest.hd, tl: ys };
  }
  return root;
};

const $$eqHelp = (x, y, depth, stack) => {
  if (x === y) return true;
  if (typeof x !== "object" || x === null || y === null) {
    if (typeof x === "function") throw new Error("Functions cannot be compared");
    return false;
  }
  if (depth > 100) {
    stack.push([x, y]);
    return true;
  }
  for (const key in x) {
    if (!$$eqHelp(x[key], y[key], depth + 1, stack)) return false;
  }
  return true;
};

const $$eq = (x, y) => {
  const stack = [];
  let equal = $$eqHelp(x, y, 0, stack);
  for (let pair = stack.pop(); equal && pair !== undefined; pair = stack.pop()) {
    equal = $$eqHelp(pair[0], pair[1], 0, stack);
  }
  return equal;
};

const $$cmp = (x, y) => {
  if (typeof x !== "object" && typeof y !== "object") {
    return x === y ? 0 : x < y ? -1 : 1;
  }
  if (Array.isArray(x)) {
    for (let index = 0; index < x.length; index++) {
      const ordering = $$cmp(x[index], y[index]);
      if (ordering !== 0) return ordering;
    }
    return 0;
  }
  let left = x;
  let right = y;
  while (left !== 0 && right !== 0) {
    const ordering = $$cmp(left.hd, right.hd);
    if (ordering !== 0) return ordering;
    left = left.tl;
    right = right.tl;
  }
  return left !== 0 ? 1 : right !== 0 ? -1 : 0;
};

const $$modBy = (modulus, x) => {
  const answer = x % modulus;
  if (modulus === 0) throw new Error("modBy: division by zero");
  return (answer > 0 && modulus < 0) || (answer < 0 && modulus > 0)
    ? answer + modulus
    : answer;
};

const $$charToCode = (char) => {
  const code = char.charCodeAt(0);
  if (0xd800 <= code && code <= 0xdbff) {
    return (code - 0xd800) * 0x400 + char.charCodeAt(1) - 0xdc00 + 0x10000;
  }
  return code;
};

const $$stringToList = (text) => {
  let list = 0;
  let index = text.length;
  while (index > 0) {
    index -= 1;
    let letter = text[index];
    const code = text.charCodeAt(index);
    if (0xdc00 <= code && code <= 0xdfff && index > 0) {
      index -= 1;
      letter = text[index] + letter;
    }
    list = { hd: letter, tl: list };
  }
  return list;
};

const $$stringFromList = (chars) => {
  let text = "";
  for (let rest = chars; rest !== 0; rest = rest.tl) text += rest.hd;
  return text;
};

const $$stringSplit = (separator, text) => {
  const parts = text.split(separator);
  let list = 0;
  for (let index = parts.length - 1; index >= 0; index--) {
    list = { hd: parts[index], tl: list };
  }
  return list;
};

const $$charFromCode = (code) => {
  if (code < 0 || 0x10ffff < code) return "\ufffd";
  if (code <= 0xffff) return String.fromCharCode(code);
  const rest = code - 0x10000;
  return String.fromCharCode(
    Math.floor(rest / 0x400) + 0xd800,
    (rest % 0x400) + 0xdc00,
  );
};

return {
  $$curry,
  $$append,
  $$eq,
  $$cmp,
  $$modBy,
  $$charToCode,
  $$charFromCode,
  $$stringToList,
  $$stringFromList,
  $$stringSplit,
};
})();

const Basics = (() => {
// Derived from elm/core -- https://github.com/elm/core
// Copyright 2014-present Evan Czaplicki, BSD 3-Clause License.
// Emitted by dartea; its LICENSE file carries the full text.
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
return { EQ, GT, LT, abs, acos, always, apL, apR, asin, atan, atan2, ceiling, clamp, compare, composeL, composeR, cos, degrees, e, floor, identity, isInfinite, $$isNaN, logBase, max, min, modBy, negate, not, pi, radians, remainderBy, round, sin, sqrt, tan, toFloat, truncate, turns, xor };
})();

const Char = (() => {
// Derived from elm/core -- https://github.com/elm/core
// Copyright 2014-present Evan Czaplicki, BSD 3-Clause License.
// Emitted by dartea; its LICENSE file carries the full text.
const toCode = x => Dartea_runtime.$$charToCode(x);
const isUpper = $$char => {
  const code = toCode($$char);
  return (code <= 90) && (65 <= code);
};
const isLower = $$char$1 => {
  const code$1 = toCode($$char$1);
  return (97 <= code$1) && (code$1 <= 122);
};
const isAlpha = $$char$2 => isLower($$char$2) || isUpper($$char$2);
const isDigit = $$char$3 => {
  const code$2 = toCode($$char$3);
  return (code$2 <= 57) && (48 <= code$2);
};
const isAlphaNum = $$char$4 => isLower($$char$4) || (isUpper($$char$4) || isDigit($$char$4));
const isOctDigit = $$char$5 => {
  const code$3 = toCode($$char$5);
  return (code$3 <= 55) && (48 <= code$3);
};
const isHexDigit = $$char$6 => {
  const code$4 = toCode($$char$6);
  return ((48 <= code$4) && (code$4 <= 57)) || (((65 <= code$4) && (code$4 <= 70)) || ((97 <= code$4) && (code$4 <= 102)));
};
const toUpper = x => x.toUpperCase();
const toLower = x => x.toLowerCase();
const fromCode = x => Dartea_runtime.$$charFromCode(x);
return { fromCode, isAlpha, isAlphaNum, isDigit, isHexDigit, isLower, isOctDigit, isUpper, toCode, toLower, toUpper };
})();

const Maybe = (() => {
// Derived from elm/core -- https://github.com/elm/core
// Copyright 2014-present Evan Czaplicki, BSD 3-Clause License.
// Emitted by dartea; its LICENSE file carries the full text.
const Just = _0 => ({ _0: _0 });
const Nothing = "Nothing";
const withDefault = ($$default, maybe) => {
  if (typeof maybe === "object") {
    const value = maybe._0;
    return value;
  } else {
    return $$default;
  }
};
const map = (f, maybe$1) => {
  if (typeof maybe$1 === "object") {
    const value$1 = maybe$1._0;
    return Just(Dartea_runtime.$$curry(f, [value$1]));
  } else {
    return Nothing;
  }
};
const andThen = (callback, maybeValue) => {
  if (typeof maybeValue === "object") {
    const value$2 = maybeValue._0;
    return callback(value$2);
  } else {
    return Nothing;
  }
};
return { Just, Nothing, andThen, map, withDefault };
})();

const List = (() => {
// Derived from elm/core -- https://github.com/elm/core
// Copyright 2014-present Evan Czaplicki, BSD 3-Clause License.
// Emitted by dartea; its LICENSE file carries the full text.
const singleton = value => ({ hd: value, tl: 0 });
const repeatHelp = (result, n, value$1) => {
  while (true) {
    if (n <= 0) {
      return result;
    } else {
      const $s1 = { hd: value$1, tl: result };
      const $s2 = n - 1;
      const $s3 = value$1;
      result = $s1;
      n = $s2;
      value$1 = $s3;
      continue;
    }
  }
};
const repeat = (n$1, value$2) => repeatHelp(0, n$1, value$2);
const rangeHelp = (lo, hi, list) => {
  while (true) {
    if (lo <= hi) {
      const $s4 = lo;
      const $s5 = hi - 1;
      const $s6 = { hd: hi, tl: list };
      lo = $s4;
      hi = $s5;
      list = $s6;
      continue;
    } else {
      return list;
    }
  }
};
const range = (lo$1, hi$1) => rangeHelp(lo$1, hi$1, 0);
const cons = (first, rest) => ({ hd: first, tl: rest });
const foldl = (func, acc, list$1) => {
  while (true) {
    if (list$1 === 0) {
      return acc;
    } else {
      const x = list$1.hd;
      const xs = list$1.tl;
      const $s7 = func;
      const $s8 = Dartea_runtime.$$curry(func, [x, acc]);
      const $s9 = xs;
      func = $s7;
      acc = $s8;
      list$1 = $s9;
      continue;
    }
  }
};
const reverse = list$2 => foldl(cons, 0, list$2);
const foldr = (func$1, acc$1, list$3) => foldl(func$1, acc$1, reverse(list$3));
const map = (f, xs$1) => {
  const func$2 = (x$1, acc$2) => {
  const first$1 = Dartea_runtime.$$curry(f, [x$1]);
  return { hd: first$1, tl: acc$2 };
};
  const acc$3 = 0;
  return foldl(func$2, acc$3, foldl(cons, 0, xs$1));
};
const length = xs$2 => foldl(($p0, count) => count + 1, 0, xs$2);
const map2Help = (f$1, xs$3, ys, acc$4) => {
  while (true) {
    if (xs$3 === 0) {
      return acc$4;
    } else {
      const x$2 = xs$3.hd;
      const xrest = xs$3.tl;
      if (ys === 0) {
        return acc$4;
      } else {
        const y = ys.hd;
        const yrest = ys.tl;
        const first$2 = Dartea_runtime.$$curry(f$1, [x$2, y]);
        const $s10 = f$1;
        const $s11 = xrest;
        const $s12 = yrest;
        const $s13 = { hd: first$2, tl: acc$4 };
        f$1 = $s10;
        xs$3 = $s11;
        ys = $s12;
        acc$4 = $s13;
        continue;
      }
    }
  }
};
const map2 = (f$2, xs$4, ys$1) => {
  const list$4 = map2Help(f$2, xs$4, ys$1, 0);
  return foldl(cons, 0, list$4);
};
const indexedMap = (f$3, xs$5) => {
  const hi$2 = length(xs$5) - 1;
  return map2(f$3, rangeHelp(0, hi$2, 0), xs$5);
};
const filter = (isGood, list$5) => foldr((x$3, acc$5) => {
  if (isGood(x$3)) {
    return { hd: x$3, tl: acc$5 };
  } else {
    return acc$5;
  }
}, 0, list$5);
const maybeCons = (f$4, mx, acc$6) => {
  const $s14 = f$4(mx);
  if (typeof $s14 === "object") {
    const value$3 = $s14._0;
    return { hd: value$3, tl: acc$6 };
  } else {
    return acc$6;
  }
};
const filterMap = (f$5, xs$6) => {
  const func$3 = ($s15, $s16) => maybeCons(f$5, $s15, $s16);
  const acc$7 = 0;
  return foldl(func$3, acc$7, foldl(cons, 0, xs$6));
};
const any = (isOkay, list$6) => {
  while (true) {
    if (list$6 === 0) {
      return false;
    } else {
      const x$4 = list$6.hd;
      const xs$7 = list$6.tl;
      if (isOkay(x$4)) {
        return true;
      } else {
        const $s17 = isOkay;
        const $s18 = xs$7;
        isOkay = $s17;
        list$6 = $s18;
        continue;
      }
    }
  }
};
const member = (x$5, xs$8) => any(a => Dartea_runtime.$$eq(a, x$5), xs$8);
const all = (isOkay$1, list$7) => Basics.not(any(a$1 => Basics.not(isOkay$1(a$1)), list$7));
const maximum = list$8 => {
  if (list$8 !== 0) {
    const x$6 = list$8.hd;
    const xs$9 = list$8.tl;
    return Maybe.Just(foldl(Basics.max, x$6, xs$9));
  } else {
    return Maybe.Nothing;
  }
};
const minimum = list$9 => {
  if (list$9 !== 0) {
    const x$7 = list$9.hd;
    const xs$10 = list$9.tl;
    return Maybe.Just(foldl(Basics.min, x$7, xs$10));
  } else {
    return Maybe.Nothing;
  }
};
const sum = numbers => foldl((x$8, acc$8) => x$8 + acc$8, 0, numbers);
const product = numbers$1 => foldl((x$9, acc$9) => x$9 * acc$9, 1, numbers$1);
const append = (xs$11, ys$2) => {
  if (ys$2 === 0) {
    return xs$11;
  } else {
    return foldl(cons, ys$2, foldl(cons, 0, xs$11));
  }
};
const concat = lists => {
  const acc$10 = 0;
  return foldl(append, acc$10, foldl(cons, 0, lists));
};
const concatMap = (f$6, list$10) => {
  const lists$1 = map(f$6, list$10);
  const acc$11 = 0;
  return foldl(append, acc$11, foldl(cons, 0, lists$1));
};
const intersperse = (sep, xs$12) => {
  if (xs$12 === 0) {
    return 0;
  } else {
    const hd = xs$12.hd;
    const tl = xs$12.tl;
    const step = (x$10, rest$1) => cons(sep, cons(x$10, rest$1));
    const acc$12 = 0;
    const spersed = foldl(step, acc$12, foldl(cons, 0, tl));
    return { hd: hd, tl: spersed };
  }
};
const map3 = (f$7, xs$13, ys$3, zs) => map2((g, z) => Dartea_runtime.$$curry(g, [z]), map2(f$7, xs$13, ys$3), zs);
const map4 = (f$8, xs$14, ys$4, zs$1, ws) => map2((g$1, w) => Dartea_runtime.$$curry(g$1, [w]), map3(f$8, xs$14, ys$4, zs$1), ws);
const map5 = (f$9, xs$15, ys$5, zs$2, ws$1, vs) => map2((g$2, v) => Dartea_runtime.$$curry(g$2, [v]), map4(f$9, xs$15, ys$5, zs$2, ws$1), vs);
const drop = (n$2, list$11) => {
  while (true) {
    if (n$2 <= 0) {
      return list$11;
    } else {
      if (list$11 === 0) {
        return list$11;
      } else {
        const xs$16 = list$11.tl;
        const $s19 = n$2 - 1;
        const $s20 = xs$16;
        n$2 = $s19;
        list$11 = $s20;
        continue;
      }
    }
  }
};
const mergeHelp = (ordering, left, right, acc$13) => {
  while (true) {
    if (left === 0) {
      return foldl(cons, acc$13, right);
    } else {
      const l = left.hd;
      const lrest = left.tl;
      if (right === 0) {
        return foldl(cons, acc$13, left);
      } else {
        const r = right.hd;
        const rrest = right.tl;
        if (ordering(r, l) === Basics.LT) {
          const $s25 = ordering;
          const $s26 = left;
          const $s27 = rrest;
          const $s28 = { hd: r, tl: acc$13 };
          ordering = $s25;
          left = $s26;
          right = $s27;
          acc$13 = $s28;
          continue;
        } else {
          const $s21 = ordering;
          const $s22 = lrest;
          const $s23 = right;
          const $s24 = { hd: l, tl: acc$13 };
          ordering = $s21;
          left = $s22;
          right = $s23;
          acc$13 = $s24;
          continue;
        }
      }
    }
  }
};
const mergeWith = (ordering$1, left$1, right$1) => {
  const list$12 = mergeHelp(ordering$1, left$1, right$1, 0);
  return foldl(cons, 0, list$12);
};
const takeHelp = (n$3, list$13, acc$14) => {
  while (true) {
    if (n$3 <= 0) {
      return acc$14;
    } else {
      if (list$13 === 0) {
        return acc$14;
      } else {
        const x$11 = list$13.hd;
        const xs$17 = list$13.tl;
        const $s29 = n$3 - 1;
        const $s30 = xs$17;
        const $s31 = { hd: x$11, tl: acc$14 };
        n$3 = $s29;
        list$13 = $s30;
        acc$14 = $s31;
        continue;
      }
    }
  }
};
const sortWith = (ordering$2, xs$18) => {
  if (xs$18 === 0) {
    return 0;
  } else {
    if (xs$18.tl === 0) {
      return xs$18;
    } else {
      const wanted = (length(xs$18) / 2) | 0;
      const list$14 = takeHelp(wanted, xs$18, 0);
      return mergeWith(ordering$2, sortWith(ordering$2, foldl(cons, 0, list$14)), sortWith(ordering$2, drop(wanted, xs$18)));
    }
  }
};
const sort = xs$19 => sortWith(Basics.compare, xs$19);
const sortBy = (toKey, xs$20) => sortWith((a$2, b) => {
  const $s32 = Dartea_runtime.$$curry(toKey, [a$2]);
  const $s33 = Dartea_runtime.$$curry(toKey, [b]);
  const $s34 = Dartea_runtime.$$cmp($s32, $s33);
  return ($s34 < 0) ? "LT" : ($s34 === 0) ? "EQ" : "GT";
}, xs$20);
const isEmpty = xs$21 => {
  if (xs$21 === 0) {
    return true;
  } else {
    return false;
  }
};
const head = list$15 => {
  if (list$15 !== 0) {
    const x$12 = list$15.hd;
    return Maybe.Just(x$12);
  } else {
    return Maybe.Nothing;
  }
};
const tail = list$16 => {
  if (list$16 !== 0) {
    const xs$22 = list$16.tl;
    return Maybe.Just(xs$22);
  } else {
    return Maybe.Nothing;
  }
};
const take = (n$4, list$17) => reverse(takeHelp(n$4, list$17, 0));
const partition = (pred, list$18) => {
  const step$1 = (x$13, $p1) => {
  const trues = $p1[0];
  const falses = $p1[1];
  if (pred(x$13)) {
    return [{ hd: x$13, tl: trues }, falses];
  } else {
    return [trues, { hd: x$13, tl: falses }];
  }
};
  return foldr(step$1, [0, 0], list$18);
};
const unzip = pairs => {
  const step$2 = ($p0$1, $p1$1) => {
  const x$14 = $p0$1[0];
  const y$1 = $p0$1[1];
  const xs$23 = $p1$1[0];
  const ys$6 = $p1$1[1];
  return [{ hd: x$14, tl: xs$23 }, { hd: y$1, tl: ys$6 }];
};
  const acc$15 = [0, 0];
  return foldl(step$2, acc$15, foldl(cons, 0, pairs));
};
return { all, any, append, concat, concatMap, cons, drop, filter, filterMap, foldl, foldr, head, indexedMap, intersperse, isEmpty, length, map, map2, map3, map4, map5, maximum, member, minimum, partition, product, range, repeat, reverse, singleton, sort, sortBy, sortWith, sum, tail, take, unzip };
})();

const Result = (() => {
// Derived from elm/core -- https://github.com/elm/core
// Copyright 2014-present Evan Czaplicki, BSD 3-Clause License.
// Emitted by dartea; its LICENSE file carries the full text.
const Ok = _0 => ({ TAG: "Ok", _0: _0 });
const Err = _0 => ({ TAG: "Err", _0: _0 });
const withDefault = (def, result) => {
  switch (result.TAG) {
    case "Ok":
      const a = result._0;
      return a;
    case "Err":
      return def;
  }
};
const map = (func, ra) => {
  switch (ra.TAG) {
    case "Ok":
      const a$1 = ra._0;
      return Ok(Dartea_runtime.$$curry(func, [a$1]));
    case "Err":
      const e = ra._0;
      return Err(e);
  }
};
const map2 = (func$1, ra$1, rb) => {
  switch (ra$1.TAG) {
    case "Err":
      const x = ra$1._0;
      return Err(x);
    case "Ok":
      const a$2 = ra$1._0;
      switch (rb.TAG) {
        case "Err":
          const x$1 = rb._0;
          return Err(x$1);
        case "Ok":
          const b = rb._0;
          return Ok(Dartea_runtime.$$curry(func$1, [a$2, b]));
      }
  }
};
const map3 = (func$2, ra$2, rb$1, rc) => {
  switch (ra$2.TAG) {
    case "Err":
      const x$2 = ra$2._0;
      return Err(x$2);
    case "Ok":
      const a$3 = ra$2._0;
      switch (rb$1.TAG) {
        case "Err":
          const x$3 = rb$1._0;
          return Err(x$3);
        case "Ok":
          const b$1 = rb$1._0;
          switch (rc.TAG) {
            case "Err":
              const x$4 = rc._0;
              return Err(x$4);
            case "Ok":
              const c = rc._0;
              return Ok(Dartea_runtime.$$curry(func$2, [a$3, b$1, c]));
          }
      }
  }
};
const map4 = (func$3, ra$3, rb$2, rc$1, rd) => map2((g, d) => Dartea_runtime.$$curry(g, [d]), map3(func$3, ra$3, rb$2, rc$1), rd);
const map5 = (func$4, ra$4, rb$3, rc$2, rd$1, re) => map2((g$1, e$1) => Dartea_runtime.$$curry(g$1, [e$1]), map4(func$4, ra$4, rb$3, rc$2, rd$1), re);
const andThen = (callback, result$1) => {
  switch (result$1.TAG) {
    case "Ok":
      const value = result$1._0;
      return callback(value);
    case "Err":
      const msg = result$1._0;
      return Err(msg);
  }
};
const mapError = (f, result$2) => {
  switch (result$2.TAG) {
    case "Ok":
      const v = result$2._0;
      return Ok(v);
    case "Err":
      const e$2 = result$2._0;
      return Err(Dartea_runtime.$$curry(f, [e$2]));
  }
};
const toMaybe = result$3 => {
  switch (result$3.TAG) {
    case "Ok":
      const v$1 = result$3._0;
      return Maybe.Just(v$1);
    case "Err":
      return Maybe.Nothing;
  }
};
const fromMaybe = (err, maybe) => {
  if (typeof maybe === "object") {
    const v$2 = maybe._0;
    return Ok(v$2);
  } else {
    return Err(err);
  }
};
return { Err, Ok, andThen, fromMaybe, map, map2, map3, map4, map5, mapError, toMaybe, withDefault };
})();

const $$String = (() => {
// Derived from elm/core -- https://github.com/elm/core
// Copyright 2014-present Evan Czaplicki, BSD 3-Clause License.
// Emitted by dartea; its LICENSE file carries the full text.
const length = x => x.length;
const append = (a, b) => a + b;
const split = (a, b) => Dartea_runtime.$$stringSplit(a, b);
const toList = x => Dartea_runtime.$$stringToList(x);
const fromList = x => Dartea_runtime.$$stringFromList(x);
const takeLeft = (a, b) => b.slice(0, a);
const dropLeftBy = (a, b) => b.slice(a);
const isEmpty = text => text === "";
const reverse = text$1 => fromList(List.reverse(toList(text$1)));
const repeatHelp = (n, chunk, result) => {
  while (true) {
    if (n <= 0) {
      return result;
    } else {
      const $s1 = n - 1;
      const $s2 = chunk;
      const $s3 = append(result, chunk);
      n = $s1;
      chunk = $s2;
      result = $s3;
      continue;
    }
  }
};
const repeat = (n$1, chunk$1) => repeatHelp(n$1, chunk$1, "");
const concat = chunks => List.foldr(append, "", chunks);
const join = (sep, chunks$1) => {
  if (chunks$1 === 0) {
    return "";
  } else {
    const first = chunks$1.hd;
    const rest = chunks$1.tl;
    return List.foldl((chunk$2, acc) => append(append(acc, sep), chunk$2), first, rest);
  }
};
const replace = (before, after, text$2) => join(after, split(before, text$2));
const isSpace = $$char => {
  const code = Char.toCode($$char);
  return (code === 32) || ((code === 10) || ((code === 13) || (code === 9)));
};
const wordStep = (letter, chunks$2) => {
  if (isSpace(letter)) {
    return { hd: "", tl: chunks$2 };
  } else {
    if (chunks$2 !== 0) {
      const current = chunks$2.hd;
      const rest$1 = chunks$2.tl;
      return { hd: append(fromList({ hd: letter, tl: 0 }), current), tl: rest$1 };
    } else {
      return { hd: fromList({ hd: letter, tl: 0 }), tl: 0 };
    }
  }
};
const words = text$3 => List.filter(chunk$3 => chunk$3 !== "", List.foldr(wordStep, { hd: "", tl: 0 }, toList(text$3)));
const lines = text$4 => split("\n", replace("\r\n", "\n", text$4));
const clampIndex = (index, size) => {
  if (index < 0) {
    const $s4 = size + index;
    return (0 > $s4) ? 0 : $s4;
  } else {
    return (index < size) ? index : size;
  }
};
const slice = (start, end, text$5) => {
  const size$1 = length(text$5);
  const from = clampIndex(start, size$1);
  const to = clampIndex(end, size$1);
  if (from >= to) {
    return "";
  } else {
    return takeLeft(to - from, dropLeftBy(from, text$5));
  }
};
const left = (n$2, text$6) => {
  if (n$2 < 1) {
    return "";
  } else {
    return takeLeft(n$2, text$6);
  }
};
const right = (n$3, text$7) => {
  if (n$3 < 1) {
    return "";
  } else {
    const $s5 = length(text$7) - n$3;
    return dropLeftBy((0 > $s5) ? 0 : $s5, text$7);
  }
};
const dropLeft = (n$4, text$8) => {
  if (n$4 < 1) {
    return text$8;
  } else {
    return dropLeftBy(n$4, text$8);
  }
};
const dropRight = (n$5, text$9) => {
  if (n$5 < 1) {
    return text$9;
  } else {
    const $s6 = length(text$9) - n$5;
    return takeLeft((0 > $s6) ? 0 : $s6, text$9);
  }
};
const contains = (needle, text$10) => {
  if (needle === "") {
    return true;
  } else {
    return List.length(split(needle, text$10)) > 1;
  }
};
const startsWith = (prefix, text$11) => left(length(prefix), text$11) === prefix;
const endsWith = (suffix, text$12) => right(length(suffix), text$12) === suffix;
const indexesHelp = (size$2, position, chunks$3, found) => {
  while (true) {
    if (chunks$3 === 0) {
      return List.reverse(found);
    } else {
      const chunk$4 = chunks$3.hd;
      const rest$2 = chunks$3.tl;
      const $s7 = size$2;
      const $s8 = (position + size$2) + length(chunk$4);
      const $s9 = rest$2;
      const $s10 = { hd: position, tl: found };
      size$2 = $s7;
      position = $s8;
      chunks$3 = $s9;
      found = $s10;
      continue;
    }
  }
};
const indexes = (needle$1, text$13) => {
  if (needle$1 === "") {
    return 0;
  } else {
    const $s11 = split(needle$1, text$13);
    if ($s11 === 0) {
      return 0;
    } else {
      const first$1 = $s11.hd;
      const rest$3 = $s11.tl;
      return indexesHelp(length(needle$1), length(first$1), rest$3, 0);
    }
  }
};
const indices = (needle$2, text$14) => indexes(needle$2, text$14);
const isInt = x => (x !== "") && Number.isInteger(Number(x));
const toIntUnsafe = x => Number(x);
const toInt = text$15 => {
  if (isInt(text$15)) {
    return Maybe.Just(toIntUnsafe(text$15));
  } else {
    return Maybe.Nothing;
  }
};
const fromInt = x => String(x);
const isFloat = x => (x !== "") && !isNaN(Number(x));
const toFloatUnsafe = x => Number(x);
const toFloat = text$16 => {
  if (isFloat(text$16)) {
    return Maybe.Just(toFloatUnsafe(text$16));
  } else {
    return Maybe.Nothing;
  }
};
const fromFloat = x => String(x);
const fromChar = $$char$1 => fromList({ hd: $$char$1, tl: 0 });
const cons = ($$char$2, text$17) => append(fromChar($$char$2), text$17);
const uncons = text$18 => {
  const $s12 = toList(text$18);
  if ($s12 === 0) {
    return Maybe.Nothing;
  } else {
    const $$char$3 = $s12.hd;
    const rest$4 = $s12.tl;
    return Maybe.Just([$$char$3, fromList(rest$4)]);
  }
};
const map = (func, text$19) => fromList(List.map(func, toList(text$19)));
const filter = (isGood, text$20) => fromList(List.filter(isGood, toList(text$20)));
const foldl = (func$1, acc$1, text$21) => List.foldl(func$1, acc$1, toList(text$21));
const foldr = (func$2, acc$2, text$22) => List.foldr(func$2, acc$2, toList(text$22));
const any = (isGood$1, text$23) => List.any(isGood$1, toList(text$23));
const all = (isGood$2, text$24) => List.all(isGood$2, toList(text$24));
const toUpper = text$25 => map(Char.toUpper, text$25);
const toLower = text$26 => map(Char.toLower, text$26);
const padLeft = (n$6, $$char$4, text$27) => append(repeat(n$6 - length(text$27), fromChar($$char$4)), text$27);
const padRight = (n$7, $$char$5, text$28) => append(text$28, repeat(n$7 - length(text$28), fromChar($$char$5)));
const pad = (n$8, $$char$6, text$29) => {
  const half = Basics.toFloat(n$8 - length(text$29)) / 2;
  const n$9 = Basics.ceiling(half);
  const chunk$5 = fromChar($$char$6);
  const n$10 = Basics.floor(half);
  const chunk$6 = fromChar($$char$6);
  return append(repeatHelp(n$9, chunk$5, ""), append(text$29, repeatHelp(n$10, chunk$6, "")));
};
const dropSpaces = chars => {
  while (true) {
    if (chars === 0) {
      return 0;
    } else {
      const $$char$7 = chars.hd;
      const rest$5 = chars.tl;
      if (isSpace($$char$7)) {
        const $s13 = rest$5;
        chars = $s13;
        continue;
      } else {
        return chars;
      }
    }
  }
};
const trimLeft = text$30 => fromList(dropSpaces(toList(text$30)));
const trimRight = text$31 => fromList(List.reverse(dropSpaces(List.reverse(toList(text$31)))));
const trim = text$32 => trimLeft(trimRight(text$32));
return { all, any, append, concat, cons, contains, dropLeft, dropRight, endsWith, filter, foldl, foldr, fromChar, fromFloat, fromInt, fromList, indexes, indices, isEmpty, join, left, length, lines, map, pad, padLeft, padRight, repeat, replace, reverse, right, slice, split, startsWith, toFloat, toInt, toList, toLower, toUpper, trim, trimLeft, trimRight, uncons, words };
})();

const Tuple = (() => {
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
return { first, pair, second };
})();

const Main = (() => {
const main = $$String.fromInt(5) + " — compiled by dartea";
return { main };
})();
export const main = Main.main;
