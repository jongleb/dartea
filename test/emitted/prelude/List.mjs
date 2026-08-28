// Compiled by dartea, an independent compiler. Not affiliated with or
// endorsed by the Elm project.
// Contains material derived from elm/core,
// Copyright 2014-present Evan Czaplicki, under the BSD 3-Clause License.
// dartea's LICENSE carries the full text.
import * as Dartea_runtime from "./Dartea_runtime.mjs";
import * as Basics from "./Basics.mjs";
import * as Maybe from "./Maybe.mjs";
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
export { all, any, append, concat, concatMap, cons, drop, filter, filterMap, foldl, foldr, head, indexedMap, intersperse, isEmpty, length, map, map2, map3, map4, map5, maximum, member, minimum, partition, product, range, repeat, reverse, singleton, sort, sortBy, sortWith, sum, tail, take, unzip };
