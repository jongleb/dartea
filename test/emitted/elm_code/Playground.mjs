import * as Dartea_runtime from "./Dartea_runtime.mjs";
import * as Basics from "./Basics.mjs";
import * as Maybe from "./Maybe.mjs";
import * as $$String from "./String.mjs";
import * as Tuple from "./Tuple.mjs";
const Ok = _0 => ({ TAG: "Ok", _0: _0 });
const Err = _0 => ({ TAG: "Err", _0: _0 });
const Lit = _0 => ({ TAG: "Lit", _0: _0 });
const Neg = _0 => ({ TAG: "Neg", _0: _0 });
const Add = (_0, _1) => ({ TAG: "Add", _0: _0, _1: _1 });
const Sub = (_0, _1) => ({ TAG: "Sub", _0: _0, _1: _1 });
const Mul = (_0, _1) => ({ TAG: "Mul", _0: _0, _1: _1 });
const Div = (_0, _1) => ({ TAG: "Div", _0: _0, _1: _1 });
const flip = (f, a, b) => Dartea_runtime.$$apply2(f, b, a);
const compose = (f$1, g, x) => Dartea_runtime.$$apply1(f$1, Dartea_runtime.$$apply1(g, x));
const twice = (f$2, eta1) => Dartea_runtime.$$apply1(f$2, Dartea_runtime.$$apply1(f$2, eta1));
const thrice = (f$3, eta1$1) => Dartea_runtime.$$apply1(f$3, Dartea_runtime.$$apply1(f$3, Dartea_runtime.$$apply1(f$3, eta1$1)));
const on = (f$4, g$1, a$1, b$1) => Dartea_runtime.$$apply2(f$4, Dartea_runtime.$$apply1(g$1, a$1), Dartea_runtime.$$apply1(g$1, b$1));
const apply = (f$5, x$1) => Dartea_runtime.$$apply1(f$5, x$1);
const neg = x$2 => 0 - x$2;
const abs = x$3 => {
  if (x$3 < 0) {
    return 0 - x$3;
  } else {
    return x$3;
  }
};
const sign = x$4 => {
  if (x$4 < 0) {
    return -1;
  } else {
    if (x$4 === 0) {
      return 0;
    } else {
      return 1;
    }
  }
};
const min = (a$2, b$2) => {
  if (Dartea_runtime.$$cmp(a$2, b$2) < 0) {
    return a$2;
  } else {
    return b$2;
  }
};
const max = (a$3, b$3) => {
  if (Dartea_runtime.$$cmp(a$3, b$3) > 0) {
    return a$3;
  } else {
    return b$3;
  }
};
const clamp = (lo, hi, x$5) => {
  const b$4 = (Dartea_runtime.$$cmp(lo, x$5) > 0) ? lo : x$5;
  if (Dartea_runtime.$$cmp(hi, b$4) < 0) {
    return hi;
  } else {
    return b$4;
  }
};
const quot = (a$4, b$5) => (a$4 / b$5) | 0;
const rem = (a$5, b$6) => a$5 - (((a$5 / b$6) | 0) * b$6);
const divides = (d, n) => rem(n, d) === 0;
const even = n$1 => rem(n$1, 2) === 0;
const odd = n$2 => (rem(n$2, 2) === 0) === false;
const gcd = (a$6, b$7) => {
  while (true) {
    if (b$7 === 0) {
      if (a$6 < 0) {
        return 0 - a$6;
      } else {
        return a$6;
      }
    } else {
      const $s1 = b$7;
      const $s2 = rem(a$6, b$7);
      a$6 = $s1;
      b$7 = $s2;
      continue;
    }
  }
};
const lcm = (a$7, b$8) => {
  const x$6 = a$7 * b$8;
  return (((x$6 < 0) ? 0 - x$6 : x$6) / gcd(a$7, b$8)) | 0;
};
const powFast = (base, e) => {
  if (e === 0) {
    return 1;
  } else {
    const half = powFast(base, (e / 2) | 0);
    const sq = half * half;
    if (rem(e, 2) === 0) {
      return sq;
    } else {
      return sq * base;
    }
  }
};
const fib = n$3 => {
  const go = (i, a$8, b$9) => {
  while (true) {
    if (i === 0) {
      return a$8;
    } else {
      const $s3 = i - 1;
      const $s4 = b$9;
      const $s5 = a$8 + b$9;
      i = $s3;
      a$8 = $s4;
      b$9 = $s5;
      continue;
    }
  }
};
  return Dartea_runtime.$$apply3(go, n$3, 0, 1);
};
const fact = n$4 => {
  if (n$4 <= 1) {
    return 1;
  } else {
    return n$4 * fact(n$4 - 1);
  }
};
const sumDigits = n$5 => {
  if (n$5 < 10) {
    return n$5;
  } else {
    return rem(n$5, 10) + sumDigits((n$5 / 10) | 0);
  }
};
const digitCount = n$6 => {
  if (n$6 < 10) {
    return 1;
  } else {
    return 1 + digitCount((n$6 / 10) | 0);
  }
};
const reverseInt = n$7 => {
  const go$1 = (m, acc) => {
  while (true) {
    if (m === 0) {
      return acc;
    } else {
      const $s6 = (m / 10) | 0;
      const $s7 = (acc * 10) + rem(m, 10);
      m = $s6;
      acc = $s7;
      continue;
    }
  }
};
  return go$1(n$7, 0);
};
const isPalindromeInt = n$8 => reverseInt(n$8) === n$8;
const isPrime = n$9 => {
  const go$2 = d$1 => {
  while (true) {
    if ((d$1 * d$1) > n$9) {
      return true;
    } else {
      if (rem(n$9, d$1) === 0) {
        return false;
      } else {
        const $s8 = d$1 + 1;
        d$1 = $s8;
        continue;
      }
    }
  }
};
  if (n$9 < 2) {
    return false;
  } else {
    return go$2(2);
  }
};
const collatzSteps = n$10 => {
  const go$3 = (m$1, acc$1) => {
  while (true) {
    if (m$1 <= 1) {
      return acc$1;
    } else {
      if (rem(m$1, 2) === 0) {
        const $s11 = (m$1 / 2) | 0;
        const $s12 = acc$1 + 1;
        m$1 = $s11;
        acc$1 = $s12;
        continue;
      } else {
        const $s9 = (3 * m$1) + 1;
        const $s10 = acc$1 + 1;
        m$1 = $s9;
        acc$1 = $s10;
        continue;
      }
    }
  }
};
  return Dartea_runtime.$$apply2(go$3, n$10, 0);
};
const ack = (m$2, n$11) => {
  while (true) {
    if (m$2 === 0) {
      return n$11 + 1;
    } else {
      if (n$11 === 0) {
        const $s15 = m$2 - 1;
        const $s16 = 1;
        m$2 = $s15;
        n$11 = $s16;
        continue;
      } else {
        const $s13 = m$2 - 1;
        const $s14 = ack(m$2, n$11 - 1);
        m$2 = $s13;
        n$11 = $s14;
        continue;
      }
    }
  }
};
const foldRange = (lo$1, hi$1, step, acc$2) => {
  while (true) {
    if (lo$1 > hi$1) {
      return acc$2;
    } else {
      const $s17 = lo$1 + 1;
      const $s18 = hi$1;
      const $s19 = step;
      const $s20 = Dartea_runtime.$$apply2(step, lo$1, acc$2);
      lo$1 = $s17;
      hi$1 = $s18;
      step = $s19;
      acc$2 = $s20;
      continue;
    }
  }
};
const sumTo = n$12 => foldRange(1, n$12, (i$1, acc$3) => i$1 + acc$3, 0);
const countPrimes = n$13 => foldRange(2, n$13, (i$2, acc$4) => {
  if (isPrime(i$2)) {
    return acc$4 + 1;
  } else {
    return acc$4;
  }
}, 0);
const mapMaybe = (f$6, m$3) => {
  if (typeof m$3 === "object") {
    const v = m$3._0;
    return Maybe.Just(Dartea_runtime.$$apply1(f$6, v));
  } else {
    return Maybe.Nothing;
  }
};
const andThen = (f$7, m$4) => {
  if (typeof m$4 === "object") {
    const v$1 = m$4._0;
    return f$7(v$1);
  } else {
    return Maybe.Nothing;
  }
};
const withDefault = (d$2, m$5) => {
  if (typeof m$5 === "object") {
    const v$2 = m$5._0;
    return v$2;
  } else {
    return d$2;
  }
};
const orElse = (fallback, m$6) => {
  if (typeof m$6 === "object") {
    const v$3 = m$6._0;
    return Maybe.Just(v$3);
  } else {
    return fallback;
  }
};
const isJust = m$7 => {
  if (typeof m$7 === "object") {
    return true;
  } else {
    return false;
  }
};
const map2 = (f$8, a$9, b$10) => {
  if (typeof a$9 === "object") {
    const v$4 = a$9._0;
    return mapMaybe(Dartea_runtime.$$apply1(f$8, v$4), b$10);
  } else {
    return Maybe.Nothing;
  }
};
const keepIf = (p, m$8) => {
  if (typeof m$8 === "object") {
    const v$5 = m$8._0;
    return (v$6 => {
  if (p(v$6)) {
    return Maybe.Just(v$6);
  } else {
    return Maybe.Nothing;
  }
})(v$5);
  } else {
    return Maybe.Nothing;
  }
};
const safeDiv = (a$10, b$11) => {
  if (b$11 === 0) {
    return Maybe.Nothing;
  } else {
    return Maybe.Just((a$10 / b$11) | 0);
  }
};
const addStrings = (a$11, b$12) => map2((x$7, y) => x$7 + y, $$String.toInt(a$11), $$String.toInt(b$12));
const $$eval = e$1 => {
  switch (e$1.TAG) {
    case "Lit":
      const n$14 = e$1._0;
      return Maybe.Just(n$14);
    case "Neg":
      const a$12 = e$1._0;
      return mapMaybe(neg, $$eval(a$12));
    case "Add":
      const a$13 = e$1._0;
      const b$13 = e$1._1;
      return map2((x$8, y$1) => x$8 + y$1, $$eval(a$13), $$eval(b$13));
    case "Sub":
      const a$14 = e$1._0;
      const b$14 = e$1._1;
      return map2((x$9, y$2) => x$9 - y$2, $$eval(a$14), $$eval(b$14));
    case "Mul":
      const a$15 = e$1._0;
      const b$15 = e$1._1;
      return map2((x$10, y$3) => x$10 * y$3, $$eval(a$15), $$eval(b$15));
    case "Div":
      const a$16 = e$1._0;
      const b$16 = e$1._1;
      const $s21 = $$eval(b$16);
      if (typeof $s21 === "object") {
        const v$7 = $s21._0;
        const y$4 = v$7;
        const $s22 = $$eval(a$16);
        if (typeof $s22 === "object") {
          const v$8 = $s22._0;
          return safeDiv(v$8, y$4);
        } else {
          return Maybe.Nothing;
        }
      } else {
        return Maybe.Nothing;
      }
  }
};
const binop = (l, op, r) => "(" + (l + (" " + (op + (" " + (r + ")")))));
const show = e$2 => {
  switch (e$2.TAG) {
    case "Lit":
      const n$15 = e$2._0;
      return $$String.fromInt(n$15);
    case "Neg":
      const a$17 = e$2._0;
      return "(0 - " + (show(a$17) + ")");
    case "Add":
      const a$18 = e$2._0;
      const b$17 = e$2._1;
      return binop(show(a$18), "+", show(b$17));
    case "Sub":
      const a$19 = e$2._0;
      const b$18 = e$2._1;
      return binop(show(a$19), "-", show(b$18));
    case "Mul":
      const a$20 = e$2._0;
      const b$19 = e$2._1;
      return binop(show(a$20), "*", show(b$19));
    case "Div":
      const a$21 = e$2._0;
      const b$20 = e$2._1;
      return binop(show(a$21), "/", show(b$20));
  }
};
const size = e$3 => {
  switch (e$3.TAG) {
    case "Lit":
      return 1;
    case "Neg":
      const a$22 = e$3._0;
      return 1 + size(a$22);
    case "Add":
      const a$23 = e$3._0;
      const b$21 = e$3._1;
      return (1 + size(a$23)) + size(b$21);
    case "Sub":
      const a$24 = e$3._0;
      const b$22 = e$3._1;
      return (1 + size(a$24)) + size(b$22);
    case "Mul":
      const a$25 = e$3._0;
      const b$23 = e$3._1;
      return (1 + size(a$25)) + size(b$23);
    case "Div":
      const a$26 = e$3._0;
      const b$24 = e$3._1;
      return (1 + size(a$26)) + size(b$24);
  }
};
const depth = e$4 => {
  switch (e$4.TAG) {
    case "Lit":
      return 1;
    case "Neg":
      const a$27 = e$4._0;
      return 1 + depth(a$27);
    case "Add":
      const a$28 = e$4._0;
      const b$25 = e$4._1;
      const eta1$2 = depth(a$28);
      const eta2 = depth(b$25);
      return 1 + ((eta1$2 > eta2) ? eta1$2 : eta2);
    case "Sub":
      const a$29 = e$4._0;
      const b$26 = e$4._1;
      const eta1$3 = depth(a$29);
      const eta2$1 = depth(b$26);
      return 1 + ((eta1$3 > eta2$1) ? eta1$3 : eta2$1);
    case "Mul":
      const a$30 = e$4._0;
      const b$27 = e$4._1;
      const eta1$4 = depth(a$30);
      const eta2$2 = depth(b$27);
      return 1 + ((eta1$4 > eta2$2) ? eta1$4 : eta2$2);
    case "Div":
      const a$31 = e$4._0;
      const b$28 = e$4._1;
      const eta1$5 = depth(a$31);
      const eta2$3 = depth(b$28);
      return 1 + ((eta1$5 > eta2$3) ? eta1$5 : eta2$3);
  }
};
const simplify = e$5 => {
  switch (e$5.TAG) {
    case "Add":
      const a$32 = e$5._0;
      const b$29 = e$5._1;
      const $s23 = [simplify(a$32), simplify(b$29)];
      const $dt0 = () => {
  const sb$1 = $s23[1];
  const sa$2 = $s23[0];
  return Add(sa$2, sb$1);
};
      if ($s23[0].TAG === "Lit") {
        if ($s23[0]._0 === 0) {
          const sb = $s23[1];
          return sb;
        } else {
          if ($s23[1].TAG === "Lit") {
            if ($s23[1]._0 === 0) {
              const sa$1 = $s23[0];
              return sa$1;
            } else {
              const y$5 = $s23[1]._0;
              const x$11 = $s23[0]._0;
              return Lit(x$11 + y$5);
            }
          } else {
            return $dt0();
          }
        }
      } else {
        if ($s23[1].TAG === "Lit") {
          if ($s23[1]._0 === 0) {
            const sa = $s23[0];
            return sa;
          } else {
            return $dt0();
          }
        } else {
          return $dt0();
        }
      }
    case "Mul":
      const a$33 = e$5._0;
      const b$30 = e$5._1;
      const $s24 = [simplify(a$33), simplify(b$30)];
      const $dt0$1 = () => Lit(0);
      const $dt1 = () => {
  const sb$5 = $s24[1];
  const sa$5 = $s24[0];
  return Mul(sa$5, sb$5);
};
      if ($s24[0].TAG === "Lit") {
        switch ($s24[0]._0) {
          case 0:
            return Lit(0);
          case 1:
            if ($s24[1].TAG === "Lit") {
              switch ($s24[1]._0) {
                case 0:
                  return $dt0$1();
                case 1:
                  const sb$4 = $s24[1];
                  return sb$4;
                default:
                  const sb$3 = $s24[1];
                  return sb$3;
              }
            } else {
              const sb$2 = $s24[1];
              return sb$2;
            }
          default:
            if ($s24[1].TAG === "Lit") {
              switch ($s24[1]._0) {
                case 0:
                  return $dt0$1();
                case 1:
                  const sa$4 = $s24[0];
                  return sa$4;
                default:
                  const y$6 = $s24[1]._0;
                  const x$12 = $s24[0]._0;
                  return Lit(x$12 * y$6);
              }
            } else {
              return $dt1();
            }
        }
      } else {
        if ($s24[1].TAG === "Lit") {
          switch ($s24[1]._0) {
            case 0:
              return $dt0$1();
            case 1:
              const sa$3 = $s24[0];
              return sa$3;
            default:
              return $dt1();
          }
        } else {
          return $dt1();
        }
      }
    case "Sub":
      const a$34 = e$5._0;
      const b$31 = e$5._1;
      const $s25 = [simplify(a$34), simplify(b$31)];
      if ($s25[1].TAG === "Lit") {
        if ($s25[1]._0 === 0) {
          const sa$8 = $s25[0];
          return sa$8;
        } else {
          if ($s25[0].TAG === "Lit") {
            const y$7 = $s25[1]._0;
            const x$13 = $s25[0]._0;
            return Lit(x$13 - y$7);
          } else {
            const sa$7 = $s25[0];
            const sb$7 = $s25[1];
            return Sub(sa$7, sb$7);
          }
        }
      } else {
        const sb$6 = $s25[1];
        const sa$6 = $s25[0];
        return Sub(sa$6, sb$6);
      }
    case "Neg":
      const a$35 = e$5._0;
      const $s26 = simplify(a$35);
      if ($s26.TAG === "Lit") {
        const x$14 = $s26._0;
        return Lit(0 - x$14);
      } else {
        const sa$9 = $s26;
        return Neg(sa$9);
      }
    case "Div":
      const a$36 = e$5._0;
      const b$32 = e$5._1;
      return Div(simplify(a$36), simplify(b$32));
    case "Lit":
      const n$16 = e$5._0;
      return Lit(n$16);
  }
};
const expr1 = Add(Mul(Lit(1), Lit(21)), Sub(Lit(21), Lit(0)));
const expr2 = Div(Lit(10), Sub(Lit(3), Lit(3)));
const expr3 = Mul(Add(Lit(2), Lit(3)), Neg(Lit(4)));
const mapRes = (f$9, r$1) => {
  switch (r$1.TAG) {
    case "Ok":
      const a$37 = r$1._0;
      return Ok(Dartea_runtime.$$apply1(f$9, a$37));
    case "Err":
      const e$6 = r$1._0;
      return Err(e$6);
  }
};
const andThenRes = (f$10, r$2) => {
  switch (r$2.TAG) {
    case "Ok":
      const a$38 = r$2._0;
      return f$10(a$38);
    case "Err":
      const e$7 = r$2._0;
      return Err(e$7);
  }
};
const resToMaybe = r$3 => {
  switch (r$3.TAG) {
    case "Ok":
      const a$39 = r$3._0;
      return Maybe.Just(a$39);
    case "Err":
      return Maybe.Nothing;
  }
};
const maybeToRes = (e$8, m$9) => {
  if (typeof m$9 === "object") {
    const a$40 = m$9._0;
    return Ok(a$40);
  } else {
    return Err(e$8);
  }
};
const evalR = e$9 => {
  switch (e$9.TAG) {
    case "Lit":
      const n$17 = e$9._0;
      return Ok(n$17);
    case "Neg":
      const a$41 = e$9._0;
      return mapRes(neg, evalR(a$41));
    case "Add":
      const a$42 = e$9._0;
      const b$33 = e$9._1;
      return andThenRes(x$15 => mapRes(y$8 => x$15 + y$8, evalR(b$33)), evalR(a$42));
    case "Sub":
      const a$43 = e$9._0;
      const b$34 = e$9._1;
      return andThenRes(x$16 => mapRes(y$9 => x$16 - y$9, evalR(b$34)), evalR(a$43));
    case "Mul":
      const a$44 = e$9._0;
      const b$35 = e$9._1;
      return andThenRes(x$17 => mapRes(y$10 => x$17 * y$10, evalR(b$35)), evalR(a$44));
    case "Div":
      const a$45 = e$9._0;
      const b$36 = e$9._1;
      return andThenRes(y$11 => {
  if (y$11 === 0) {
    return Err("div by zero in " + show(e$9));
  } else {
    return mapRes(x$18 => (x$18 / y$11) | 0, evalR(a$45));
  }
}, evalR(b$36));
  }
};
const showRes = r$4 => {
  switch (r$4.TAG) {
    case "Ok":
      const n$18 = r$4._0;
      return "ok " + $$String.fromInt(n$18);
    case "Err":
      const msg = r$4._0;
      return "err " + msg;
  }
};
const vec = (x$19, y$12) => Tuple.pair(x$19, y$12);
const vx = v$9 => Tuple.first(v$9);
const vy = v$10 => Tuple.second(v$10);
const vadd = (a$46, b$37) => Tuple.pair(Tuple.first(a$46) + Tuple.first(b$37), Tuple.second(a$46) + Tuple.second(b$37));
const vsub = (a$47, b$38) => Tuple.pair(Tuple.first(a$47) - Tuple.first(b$38), Tuple.second(a$47) - Tuple.second(b$38));
const vscale = (k, v$11) => Tuple.pair(k * Tuple.first(v$11), k * Tuple.second(v$11));
const vdot = (a$48, b$39) => (Tuple.first(a$48) * Tuple.first(b$39)) + (Tuple.second(a$48) * Tuple.second(b$39));
const vlen2 = v$12 => vdot(v$12, v$12);
const manhattan = (a$49, b$40) => {
  const x$20 = Tuple.first(a$49) - Tuple.first(b$40);
  const x$21 = Tuple.second(a$49) - Tuple.second(b$40);
  return ((x$20 < 0) ? 0 - x$20 : x$20) + ((x$21 < 0) ? 0 - x$21 : x$21);
};
const vshow = v$13 => "(" + ($$String.fromInt(Tuple.first(v$13)) + (", " + ($$String.fromInt(Tuple.second(v$13)) + ")")));
const swap = t => Tuple.pair(Tuple.second(t), Tuple.first(t));
const nil = ($p0, z) => z;
const cons = (x$22, xs, f$11, z$1) => Dartea_runtime.$$apply2(f$11, x$22, Dartea_runtime.$$apply2(xs, f$11, z$1));
const foldr = (f$12, z$2, xs$1) => Dartea_runtime.$$apply2(xs$1, f$12, z$2);
const lmap = (f$13, xs$2, g$2, z$3) => Dartea_runtime.$$apply2(xs$2, (x$23, acc$5) => Dartea_runtime.$$apply2(g$2, Dartea_runtime.$$apply1(f$13, x$23), acc$5), z$3);
const lfilter = (p$1, xs$3, g$3, z$4) => Dartea_runtime.$$apply2(xs$3, (x$24, acc$6) => {
  if (p$1(x$24)) {
    return Dartea_runtime.$$apply2(g$3, x$24, acc$6);
  } else {
    return acc$6;
  }
}, z$4);
const lsum = xs$4 => Dartea_runtime.$$apply2(xs$4, (x$25, acc$7) => x$25 + acc$7, 0);
const llen = xs$5 => Dartea_runtime.$$apply2(xs$5, ($p0$1, acc$8) => acc$8 + 1, 0);
const lall = (p$2, xs$6) => Dartea_runtime.$$apply2(xs$6, (x$26, acc$9) => p$2(x$26) && acc$9, true);
const lany = (p$3, xs$7) => Dartea_runtime.$$apply2(xs$7, (x$27, acc$10) => p$3(x$27) || acc$10, false);
const lrange = (lo$2, hi$2, eta1$6, eta2$4) => Dartea_runtime.$$apply2((lo$2 > hi$2) ? nil : ($s29, $s30) => cons(lo$2, ($s27, $s28) => lrange(lo$2 + 1, hi$2, $s27, $s28), $s29, $s30), eta1$6, eta2$4);
const ljoin = (sep, xs$8) => Dartea_runtime.$$apply2(xs$8, (x$28, acc$11) => {
  if (acc$11 === "") {
    return x$28;
  } else {
    return x$28 + (sep + acc$11);
  }
}, "");
const lshow = xs$9 => "[" + (ljoin(", ", ($s31, $s32) => lmap($$String.fromInt, xs$9, $s31, $s32)) + "]");
const primesTo = (n$19, eta1$7, eta2$5) => lfilter(isPrime, ($s33, $s34) => lrange(2, n$19, $s33, $s34), eta1$7, eta2$5);
const squares = (n$20, eta1$8, eta2$6) => lmap(x$29 => x$29 * x$29, ($s35, $s36) => lrange(1, n$20, $s35, $s36), eta1$8, eta2$6);
const fizzbuzz = n$21 => {
  if (rem(n$21, 15) === 0) {
    return "FizzBuzz";
  } else {
    if (rem(n$21, 3) === 0) {
      return "Fizz";
    } else {
      if (rem(n$21, 5) === 0) {
        return "Buzz";
      } else {
        return $$String.fromInt(n$21);
      }
    }
  }
};
const fizzbuzzTo = n$22 => ljoin(" ", ($s39, $s40) => lmap(fizzbuzz, ($s37, $s38) => lrange(1, n$22, $s37, $s38), $s39, $s40));
const polyTest = Tuple.pair(Tuple.pair(Basics.identity(1), Basics.identity("str")), Tuple.pair(Basics.always(true, "x"), Basics.always("y", 42)));
const line = (label, value) => label + (" = " + value);
const report = ljoin("; ", ($s57, $s58) => cons("fib 25" + (" = " + $$String.fromInt(fib(25))), ($s55, $s56) => cons("fact 10" + (" = " + $$String.fromInt(fact(10))), ($s53, $s54) => cons("gcd 462 1071" + (" = " + $$String.fromInt(gcd(462, 1071))), ($s51, $s52) => cons("2^16" + (" = " + $$String.fromInt(powFast(2, 16))), ($s49, $s50) => cons("collatz 27" + (" = " + $$String.fromInt(collatzSteps(27))), ($s47, $s48) => cons("primes<100" + (" = " + $$String.fromInt(countPrimes(100))), ($s45, $s46) => cons("ack 2 3" + (" = " + $$String.fromInt(ack(2, 3))), ($s43, $s44) => cons("sumDigits 987654" + (" = " + $$String.fromInt(sumDigits(987654))), ($s41, $s42) => cons("palindrome 12321" + (" = " + $$String.fromInt(reverseInt(12321))), nil, $s41, $s42), $s43, $s44), $s45, $s46), $s47, $s48), $s49, $s50), $s51, $s52), $s53, $s54), $s55, $s56), $s57, $s58));
const $s59 = $$eval(expr1);
let $s60;
if (typeof $s59 === "object") {
  const v$14 = $s59._0;
  $s60 = v$14;
} else {
  $s60 = 0;
}
const demoExpr1 = show(simplify(expr1)) + (" -> " + $$String.fromInt($s60));
const demoExpr2 = showRes(evalR(expr2));
const demoExpr3 = show(simplify(expr3));
const demoVec = vshow(vadd(Tuple.pair(3, 4), vscale(2, Tuple.pair(1, 1))));
const demoList = lshow(($s61, $s62) => squares(10, $s61, $s62)) + (" sum=" + $$String.fromInt(squares(10, (x$30, acc$12) => x$30 + acc$12, 0)));
const demoPrimes = lshow(($s63, $s64) => primesTo(50, $s63, $s64));
const demoFizz = fizzbuzzTo(20);
const $s65 = addStrings("40", "2");
let $s66;
if (typeof $s65 === "object") {
  const v$15 = $s65._0;
  $s66 = v$15;
} else {
  $s66 = 0;
}
const demoMaybe = $s66;
const $s67 = mapMaybe(x$31 => x$31 * 6, $$String.toInt("7"));
let $s68;
if (typeof $s67 === "object") {
  const v$16 = $s67._0;
  $s68 = v$16;
} else {
  $s68 = 0;
}
const demoChain = $$String.fromInt($s68);
export { Add, Div, Err, Lit, Mul, Neg, Ok, Sub, abs, ack, addStrings, andThen, andThenRes, apply, binop, clamp, collatzSteps, compose, cons, countPrimes, demoChain, demoExpr1, demoExpr2, demoExpr3, demoFizz, demoList, demoMaybe, demoPrimes, demoVec, depth, digitCount, divides, $$eval, evalR, even, expr1, expr2, expr3, fact, fib, fizzbuzz, fizzbuzzTo, flip, foldRange, foldr, gcd, isJust, isPalindromeInt, isPrime, keepIf, lall, lany, lcm, lfilter, line, ljoin, llen, lmap, lrange, lshow, lsum, manhattan, map2, mapMaybe, mapRes, max, maybeToRes, min, neg, nil, odd, on, orElse, polyTest, powFast, primesTo, quot, rem, report, resToMaybe, reverseInt, safeDiv, show, showRes, sign, simplify, size, squares, sumDigits, sumTo, swap, thrice, twice, vadd, vdot, vec, vlen2, vscale, vshow, vsub, vx, vy, withDefault };
