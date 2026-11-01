import * as Dartea_runtime from "./Dartea_runtime.mjs";
import * as Dartea_browser from "./Dartea_browser.mjs";
import * as Basics from "./Basics.mjs";
import * as Bitwise from "./Bitwise.mjs";
import * as List from "./List.mjs";
import * as Task from "./Task.mjs";
const Seed = (_0, _1) => ({ _0: _0, _1: _1 });
const Generator = _0 => ({ _0: _0 });
const next = $p0 => {
  const state0 = $p0._0;
  const incr = $p0._1;
  return Seed(Bitwise.shiftRightZfBy(0, (state0 * 1664525) + incr), incr);
};
const peel = $p0$1 => {
  const state = $p0$1._0;
  const word = Bitwise.xor(state, Bitwise.shiftRightZfBy(Bitwise.shiftRightZfBy(28, state) + 4, state)) * 277803737;
  return Bitwise.shiftRightZfBy(0, Bitwise.xor(Bitwise.shiftRightZfBy(22, word), word));
};
const $$int = (a, b) => Generator(seed0 => {
  const $s1 = (a < b) ? [a, b] : [b, a];
  const lo = $s1[0];
  const hi = $s1[1];
  const range = (hi - lo) + 1;
  if (Bitwise.and(range - 1, range) === 0) {
    return [Bitwise.shiftRightZfBy(0, Bitwise.and(range - 1, peel(seed0))) + lo, next(seed0)];
  } else {
    const threshhold = Bitwise.shiftRightZfBy(0, Basics.remainderBy(range, Bitwise.shiftRightZfBy(0, Basics.negate(range))));
    const accountForBias = seed => {
  while (true) {
    const x = peel(seed);
    const seedN = next(seed);
    if (x < threshhold) {
      const $s2 = seedN;
      seed = $s2;
      continue;
    } else {
      return [Basics.remainderBy(range, x) + lo, seedN];
    }
  }
};
    return accountForBias(seed0);
  }
});
const maxInt = 2147483647;
const minInt = Basics.negate(2147483648);
const $$float = (a$1, b$1) => Generator(seed0$1 => {
  const seed1 = next(seed0$1);
  const n0 = peel(seed0$1);
  const n1 = peel(seed1);
  const hi$1 = Basics.toFloat(Bitwise.and(67108863, n0)) * 1.0;
  const lo$1 = Basics.toFloat(Bitwise.and(134217727, n1)) * 1.0;
  const val = ((hi$1 * 134217728.0) + lo$1) / 9007199254740992.0;
  const range$1 = Basics.abs(b$1 - a$1);
  const scaled = (val * range$1) + a$1;
  return [scaled, next(seed1)];
});
const constant = value => Generator(seed$1 => [value, seed$1]);
const map2 = (func, $p1, $p2) => {
  const genA = $p1._0;
  const genB = $p2._0;
  return Generator(seed0$2 => {
  const $s3 = genA(seed0$2);
  const a$2 = $s3[0];
  const seed1$1 = $s3[1];
  const $s4 = genB(seed1$1);
  const b$2 = $s4[0];
  const seed2 = $s4[1];
  return [Dartea_runtime.$$apply2(func, a$2, b$2), seed2];
});
};
const pair = (genA$1, genB$1) => map2((a$3, b$3) => [a$3, b$3], genA$1, genB$1);
const listHelp = (revList, n, gen, seed$2) => {
  while (true) {
    if (n < 1) {
      return [revList, seed$2];
    } else {
      const $s5 = gen(seed$2);
      const value$1 = $s5[0];
      const newSeed = $s5[1];
      const $s6 = { hd: value$1, tl: revList };
      const $s7 = n - 1;
      const $s8 = gen;
      const $s9 = newSeed;
      revList = $s6;
      n = $s7;
      gen = $s8;
      seed$2 = $s9;
      continue;
    }
  }
};
const list = (n$1, $p1$1) => {
  const gen$1 = $p1$1._0;
  return Generator(seed$3 => listHelp(0, n$1, gen$1, seed$3));
};
const addOne = value$2 => [1, value$2];
const getByWeight = ($p0$2, others, countdown) => {
  while (true) {
    const weight = $p0$2[0];
    const value$3 = $p0$2[1];
    if (others === 0) {
      return value$3;
    } else {
      const second = others.hd;
      const otherOthers = others.tl;
      if (countdown <= Basics.abs(weight)) {
        return value$3;
      } else {
        const $s10 = second;
        const $s11 = otherOthers;
        const $s12 = countdown - Basics.abs(weight);
        $p0$2 = $s10;
        others = $s11;
        countdown = $s12;
        continue;
      }
    }
  }
};
const map = (func$1, $p1$2) => {
  const genA$2 = $p1$2._0;
  return Generator(seed0$3 => {
  const $s13 = genA$2(seed0$3);
  const a$4 = $s13[0];
  const seed1$2 = $s13[1];
  return [Dartea_runtime.$$apply1(func$1, a$4), seed1$2];
});
};
const weighted = (first, others$1) => {
  const normalize = $p0$3 => {
  const weight$1 = $p0$3[0];
  return Basics.abs(weight$1);
};
  const total = normalize(first) + List.sum(List.map(normalize, others$1));
  return map($s14 => getByWeight(first, others$1, $s14), $$float(0, total));
};
const uniform = (value$4, valueList) => weighted([1, value$4], List.map(addOne, valueList));
const map3 = (func$2, $p1$3, $p2$1, $p3) => {
  const genA$3 = $p1$3._0;
  const genB$2 = $p2$1._0;
  const genC = $p3._0;
  return Generator(seed0$4 => {
  const $s15 = genA$3(seed0$4);
  const a$5 = $s15[0];
  const seed1$3 = $s15[1];
  const $s16 = genB$2(seed1$3);
  const b$4 = $s16[0];
  const seed2$1 = $s16[1];
  const $s17 = genC(seed2$1);
  const c = $s17[0];
  const seed3 = $s17[1];
  return [Dartea_runtime.$$apply3(func$2, a$5, b$4, c), seed3];
});
};
const map4 = (func$3, $p1$4, $p2$2, $p3$1, $p4) => {
  const genA$4 = $p1$4._0;
  const genB$3 = $p2$2._0;
  const genC$1 = $p3$1._0;
  const genD = $p4._0;
  return Generator(seed0$5 => {
  const $s18 = genA$4(seed0$5);
  const a$6 = $s18[0];
  const seed1$4 = $s18[1];
  const $s19 = genB$3(seed1$4);
  const b$5 = $s19[0];
  const seed2$2 = $s19[1];
  const $s20 = genC$1(seed2$2);
  const c$1 = $s20[0];
  const seed3$1 = $s20[1];
  const $s21 = genD(seed3$1);
  const d = $s21[0];
  const seed4 = $s21[1];
  return [Dartea_runtime.$$apply4(func$3, a$6, b$5, c$1, d), seed4];
});
};
const map5 = (func$4, $p1$5, $p2$3, $p3$2, $p4$1, $p5) => {
  const genA$5 = $p1$5._0;
  const genB$4 = $p2$3._0;
  const genC$2 = $p3$2._0;
  const genD$1 = $p4$1._0;
  const genE = $p5._0;
  return Generator(seed0$6 => {
  const $s22 = genA$5(seed0$6);
  const a$7 = $s22[0];
  const seed1$5 = $s22[1];
  const $s23 = genB$4(seed1$5);
  const b$6 = $s23[0];
  const seed2$3 = $s23[1];
  const $s24 = genC$2(seed2$3);
  const c$2 = $s24[0];
  const seed3$2 = $s24[1];
  const $s25 = genD$1(seed3$2);
  const d$1 = $s25[0];
  const seed4$1 = $s25[1];
  const $s26 = genE(seed4$1);
  const e = $s26[0];
  const seed5 = $s26[1];
  return [Dartea_runtime.$$apply5(func$4, a$7, b$6, c$2, d$1, e), seed5];
});
};
const andThen = (callback, $p1$6) => {
  const genA$6 = $p1$6._0;
  return Generator(seed$4 => {
  const $s27 = genA$6(seed$4);
  const result = $s27[0];
  const newSeed$1 = $s27[1];
  const $s28 = callback(result);
  const genB$5 = $s28._0;
  return genB$5(newSeed$1);
});
};
const lazy = callback$1 => Generator(seed$5 => {
  const $s29 = callback$1(null);
  const gen$2 = $s29._0;
  return gen$2(seed$5);
});
const step = ($p0$4, seed$6) => {
  const generator = $p0$4._0;
  return generator(seed$6);
};
const initialSeed = x$1 => {
  const $s30 = next(Seed(0, 1013904223));
  const state1 = $s30._0;
  const incr$1 = $s30._1;
  const state2 = Bitwise.shiftRightZfBy(0, state1 + x$1);
  return next(Seed(state2, incr$1));
};
const independentSeed = Generator(seed0$7 => {
  const gen$3 = $$int(0, 4294967295);
  const makeIndependentSeed = (state$1, b$7, c$3) => next(Seed(state$1, Bitwise.shiftRightZfBy(0, Bitwise.or(1, Bitwise.xor(b$7, c$3)))));
  const $s31 = map3(makeIndependentSeed, gen$3, gen$3, gen$3);
  const generator$1 = $s31._0;
  return generator$1(seed0$7);
});
const generate = (tagger, $p1$7) => {
  const generator$2 = $p1$7._0;
  return Task.perform(tagger, Dartea_browser.$$Random$generate(initialSeed, generator$2));
};
export { andThen, constant, $$float, generate, independentSeed, initialSeed, $$int, lazy, list, map, map2, map3, map4, map5, maxInt, minInt, pair, step, uniform, weighted };
