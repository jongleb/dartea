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
      const $s8 = Dartea_runtime.$$apply2(func, x, acc);
      const $s9 = xs;
      func = $s7;
      acc = $s8;
      list$1 = $s9;
      continue;
    }
  }
};
const foldr = (func$1, acc$1, list$2) => foldl(func$1, acc$1, Dartea_runtime.$$listReverse(list$2));
const reverse = list$3 => Dartea_runtime.$$listReverse(list$3);
const map = (f, xs$1) => Dartea_runtime.$$listMap(f, xs$1);
const map2Help = (f$1, xs$2, ys, acc$2) => {
  while (true) {
    if (xs$2 === 0) {
      return acc$2;
    } else {
      const x$1 = xs$2.hd;
      const xrest = xs$2.tl;
      if (ys === 0) {
        return acc$2;
      } else {
        const y = ys.hd;
        const yrest = ys.tl;
        const $s10 = f$1;
        const $s11 = xrest;
        const $s12 = yrest;
        const $s13 = { hd: Dartea_runtime.$$apply2(f$1, x$1, y), tl: acc$2 };
        f$1 = $s10;
        xs$2 = $s11;
        ys = $s12;
        acc$2 = $s13;
        continue;
      }
    }
  }
};
const indexedMap = (f$2, xs$3) => Dartea_runtime.$$listReverse(map2Help(f$2, rangeHelp(0, Dartea_runtime.$$listLength(xs$3) - 1, 0), xs$3, 0));
const filter = (isGood, list$4) => Dartea_runtime.$$listFilter(isGood, list$4);
const maybeCons = (f$3, mx, acc$3) => {
  const $s14 = f$3(mx);
  if (typeof $s14 === "object") {
    const value$3 = $s14._0;
    return { hd: value$3, tl: acc$3 };
  } else {
    return acc$3;
  }
};
const filterMap = (f$4, xs$4) => foldl(($s15, $s16) => maybeCons(f$4, $s15, $s16), 0, Dartea_runtime.$$listReverse(xs$4));
const length = xs$5 => Dartea_runtime.$$listLength(xs$5);
const any = (isOkay, list$5) => {
  while (true) {
    if (list$5 === 0) {
      return false;
    } else {
      const x$2 = list$5.hd;
      const xs$6 = list$5.tl;
      if (isOkay(x$2)) {
        return true;
      } else {
        const $s17 = isOkay;
        const $s18 = xs$6;
        isOkay = $s17;
        list$5 = $s18;
        continue;
      }
    }
  }
};
const member = (x$3, xs$7) => any(a => Dartea_runtime.$$eq(a, x$3), xs$7);
const all = (isOkay$1, list$6) => Basics.not(any(a$1 => Basics.not(isOkay$1(a$1)), list$6));
const maximum = list$7 => {
  if (list$7 !== 0) {
    const x$4 = list$7.hd;
    const xs$8 = list$7.tl;
    return Maybe.Just(foldl(Basics.max, x$4, xs$8));
  } else {
    return Maybe.Nothing;
  }
};
const minimum = list$8 => {
  if (list$8 !== 0) {
    const x$5 = list$8.hd;
    const xs$9 = list$8.tl;
    return Maybe.Just(foldl(Basics.min, x$5, xs$9));
  } else {
    return Maybe.Nothing;
  }
};
const sum = numbers => foldl((x$6, acc$4) => x$6 + acc$4, 0, numbers);
const product = numbers$1 => foldl((x$7, acc$5) => x$7 * acc$5, 1, numbers$1);
const append = (xs$10, ys$1) => {
  if (ys$1 === 0) {
    return xs$10;
  } else {
    return foldl(cons, ys$1, Dartea_runtime.$$listReverse(xs$10));
  }
};
const concat = lists => foldl(append, 0, Dartea_runtime.$$listReverse(lists));
const concatMap = (f$5, list$9) => foldl(append, 0, Dartea_runtime.$$listReverse(Dartea_runtime.$$listMap(f$5, list$9)));
const intersperse = (sep, xs$11) => {
  if (xs$11 === 0) {
    return 0;
  } else {
    const hd = xs$11.hd;
    const tl = xs$11.tl;
    const step = (x$8, rest$1) => ({ hd: sep, tl: { hd: x$8, tl: rest$1 } });
    const spersed = foldl(step, 0, Dartea_runtime.$$listReverse(tl));
    return { hd: hd, tl: spersed };
  }
};
const map2 = (f$6, xs$12, ys$2) => Dartea_runtime.$$listReverse(map2Help(f$6, xs$12, ys$2, 0));
const map3 = (f$7, xs$13, ys$3, zs) => Dartea_runtime.$$listReverse(map2Help((g, z) => Dartea_runtime.$$apply1(g, z), Dartea_runtime.$$listReverse(map2Help(f$7, xs$13, ys$3, 0)), zs, 0));
const map4 = (f$8, xs$14, ys$4, zs$1, ws) => Dartea_runtime.$$listReverse(map2Help((g$1, w) => Dartea_runtime.$$apply1(g$1, w), map3(f$8, xs$14, ys$4, zs$1), ws, 0));
const map5 = (f$9, xs$15, ys$5, zs$2, ws$1, vs) => Dartea_runtime.$$listReverse(map2Help((g$2, v) => Dartea_runtime.$$apply1(g$2, v), map4(f$9, xs$15, ys$5, zs$2, ws$1), vs, 0));
const drop = (n$2, list$10) => {
  while (true) {
    if (n$2 <= 0) {
      return list$10;
    } else {
      if (list$10 === 0) {
        return list$10;
      } else {
        const xs$16 = list$10.tl;
        const $s19 = n$2 - 1;
        const $s20 = xs$16;
        n$2 = $s19;
        list$10 = $s20;
        continue;
      }
    }
  }
};
const mergeHelp = (ordering, left, right, acc$6) => {
  while (true) {
    if (left === 0) {
      return foldl(cons, acc$6, right);
    } else {
      const l = left.hd;
      const lrest = left.tl;
      if (right === 0) {
        return foldl(cons, acc$6, left);
      } else {
        const r = right.hd;
        const rrest = right.tl;
        if (ordering(r, l) === Basics.LT) {
          const $s25 = ordering;
          const $s26 = left;
          const $s27 = rrest;
          const $s28 = { hd: r, tl: acc$6 };
          ordering = $s25;
          left = $s26;
          right = $s27;
          acc$6 = $s28;
          continue;
        } else {
          const $s21 = ordering;
          const $s22 = lrest;
          const $s23 = right;
          const $s24 = { hd: l, tl: acc$6 };
          ordering = $s21;
          left = $s22;
          right = $s23;
          acc$6 = $s24;
          continue;
        }
      }
    }
  }
};
const takeHelp = (n$3, list$11, acc$7) => {
  while (true) {
    if (n$3 <= 0) {
      return acc$7;
    } else {
      if (list$11 === 0) {
        return acc$7;
      } else {
        const x$9 = list$11.hd;
        const xs$17 = list$11.tl;
        const $s29 = n$3 - 1;
        const $s30 = xs$17;
        const $s31 = { hd: x$9, tl: acc$7 };
        n$3 = $s29;
        list$11 = $s30;
        acc$7 = $s31;
        continue;
      }
    }
  }
};
const sortWith = (ordering$1, xs$18) => {
  if (xs$18 === 0) {
    return 0;
  } else {
    if (xs$18.tl === 0) {
      return xs$18;
    } else {
      const wanted = (Dartea_runtime.$$listLength(xs$18) / 2) | 0;
      return Dartea_runtime.$$listReverse(mergeHelp(ordering$1, sortWith(ordering$1, Dartea_runtime.$$listReverse(takeHelp(wanted, xs$18, 0))), sortWith(ordering$1, drop(wanted, xs$18)), 0));
    }
  }
};
const sort = xs$19 => sortWith(Basics.compare, xs$19);
const sortBy = (toKey, xs$20) => sortWith((a$2, b) => {
  const $s32 = Dartea_runtime.$$apply1(toKey, a$2);
  const $s33 = Dartea_runtime.$$apply1(toKey, b);
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
const head = list$12 => {
  if (list$12 !== 0) {
    const x$10 = list$12.hd;
    return Maybe.Just(x$10);
  } else {
    return Maybe.Nothing;
  }
};
const tail = list$13 => {
  if (list$13 !== 0) {
    const xs$22 = list$13.tl;
    return Maybe.Just(xs$22);
  } else {
    return Maybe.Nothing;
  }
};
const take = (n$4, list$14) => Dartea_runtime.$$listReverse(takeHelp(n$4, list$14, 0));
const partition = (pred, list$15) => {
  const step$1 = (x$11, $p1) => {
  const trues = $p1[0];
  const falses = $p1[1];
  if (pred(x$11)) {
    return [{ hd: x$11, tl: trues }, falses];
  } else {
    return [trues, { hd: x$11, tl: falses }];
  }
};
  return foldl(step$1, [0, 0], Dartea_runtime.$$listReverse(list$15));
};
const unzip = pairs => {
  const step$2 = ($p0, $p1$1) => {
  const x$12 = $p0[0];
  const y$1 = $p0[1];
  const xs$23 = $p1$1[0];
  const ys$6 = $p1$1[1];
  return [{ hd: x$12, tl: xs$23 }, { hd: y$1, tl: ys$6 }];
};
  return foldl(step$2, [0, 0], Dartea_runtime.$$listReverse(pairs));
};
export { all, any, append, concat, concatMap, cons, drop, filter, filterMap, foldl, foldr, head, indexedMap, intersperse, isEmpty, length, map, map2, map3, map4, map5, maximum, member, minimum, partition, product, range, repeat, reverse, singleton, sort, sortBy, sortWith, sum, tail, take, unzip };
