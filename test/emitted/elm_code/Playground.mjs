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
const flip = (f, a, b) => Dartea_runtime.$$curry(f, [b, a]);
const compose = (f$1, g, x) => Dartea_runtime.$$curry(f$1, [Dartea_runtime.$$curry(g, [x])]);
const twice = (f$2, eta1) => compose(f$2, f$2, eta1);
const thrice = (f$3, eta1$1) => compose(f$3, $s1 => twice(f$3, $s1), eta1$1);
const on = (f$4, g$1, a$1, b$1) => Dartea_runtime.$$curry(f$4, [Dartea_runtime.$$curry(g$1, [a$1]), Dartea_runtime.$$curry(g$1, [b$1])]);
const apply = (f$5, x$1) => Dartea_runtime.$$curry(f$5, [x$1]);
const neg = x$2 => 0 - x$2;
const abs = x$3 => {
  if (x$3 < 0) {
    return neg(x$3);
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
const rem = (a$5, b$6) => a$5 - (quot(a$5, b$6) * b$6);
const divides = (d, n) => rem(n, d) === 0;
const even = n$1 => divides(2, n$1);
const odd = n$2 => even(n$2) === false;
const gcd = (a$6, b$7) => {
  while (true) {
    if (b$7 === 0) {
      if (a$6 < 0) {
        return 0 - a$6;
      } else {
        return a$6;
      }
    } else {
      const $s2 = b$7;
      const $s3 = rem(a$6, b$7);
      a$6 = $s2;
      b$7 = $s3;
      continue;
    }
  }
};
const lcm = (a$7, b$8) => {
  const x$6 = a$7 * b$8;
  return quot((x$6 < 0) ? neg(x$6) : x$6, gcd(a$7, b$8));
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
  if (i === 0) {
    return a$8;
  } else {
    return Dartea_runtime.$$curry(go, [i - 1, b$9, a$8 + b$9]);
  }
};
  return Dartea_runtime.$$curry(go, [n$3, 0, 1]);
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
  if (m === 0) {
    return acc;
  } else {
    return go$1((m / 10) | 0, (acc * 10) + rem(m, 10));
  }
};
  return go$1(n$7, 0);
};
const isPalindromeInt = n$8 => reverseInt(n$8) === n$8;
const isPrime = n$9 => {
  const go$2 = d$1 => {
  if ((d$1 * d$1) > n$9) {
    return true;
  } else {
    if (divides(d$1, n$9)) {
      return false;
    } else {
      return go$2(d$1 + 1);
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
  if (m$1 <= 1) {
    return acc$1;
  } else {
    if (rem(m$1, 2) === 0) {
      return Dartea_runtime.$$curry(go$3, [(m$1 / 2) | 0, acc$1 + 1]);
    } else {
      return Dartea_runtime.$$curry(go$3, [(3 * m$1) + 1, acc$1 + 1]);
    }
  }
};
  return Dartea_runtime.$$curry(go$3, [n$10, 0]);
};
const ack = (m$2, n$11) => {
  while (true) {
    if (m$2 === 0) {
      return n$11 + 1;
    } else {
      if (n$11 === 0) {
        const $s6 = m$2 - 1;
        const $s7 = 1;
        m$2 = $s6;
        n$11 = $s7;
        continue;
      } else {
        const $s4 = m$2 - 1;
        const $s5 = ack(m$2, n$11 - 1);
        m$2 = $s4;
        n$11 = $s5;
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
      const $s8 = lo$1 + 1;
      const $s9 = hi$1;
      const $s10 = step;
      const $s11 = Dartea_runtime.$$curry(step, [lo$1, acc$2]);
      lo$1 = $s8;
      hi$1 = $s9;
      step = $s10;
      acc$2 = $s11;
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
    return Maybe.Just(Dartea_runtime.$$curry(f$6, [v]));
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
const map2 = (f$8, a$9, b$10) => andThen(x$7 => mapMaybe(Dartea_runtime.$$curry(f$8, [x$7]), b$10), a$9);
const keepIf = (p, m$8) => andThen(v$4 => {
  if (p(v$4)) {
    return Maybe.Just(v$4);
  } else {
    return Maybe.Nothing;
  }
}, m$8);
const safeDiv = (a$10, b$11) => {
  if (b$11 === 0) {
    return Maybe.Nothing;
  } else {
    return Maybe.Just(quot(a$10, b$11));
  }
};
const addStrings = (a$11, b$12) => map2((x$8, y) => x$8 + y, $$String.toInt(a$11), $$String.toInt(b$12));
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
      return map2((x$9, y$1) => x$9 + y$1, $$eval(a$13), $$eval(b$13));
    case "Sub":
      const a$14 = e$1._0;
      const b$14 = e$1._1;
      return map2((x$10, y$2) => x$10 - y$2, $$eval(a$14), $$eval(b$14));
    case "Mul":
      const a$15 = e$1._0;
      const b$15 = e$1._1;
      return map2((x$11, y$3) => x$11 * y$3, $$eval(a$15), $$eval(b$15));
    case "Div":
      const a$16 = e$1._0;
      const b$16 = e$1._1;
      const f$9 = y$4 => {
  const f$10 = x$12 => safeDiv(x$12, y$4);
  const m$9 = $$eval(a$16);
  if (typeof m$9 === "object") {
    const v$5 = m$9._0;
    return f$10(v$5);
  } else {
    return Maybe.Nothing;
  }
};
      const m$10 = $$eval(b$16);
      if (typeof m$10 === "object") {
        const v$6 = m$10._0;
        return f$9(v$6);
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
      return 1 + max(depth(a$28), depth(b$25));
    case "Sub":
      const a$29 = e$4._0;
      const b$26 = e$4._1;
      return 1 + max(depth(a$29), depth(b$26));
    case "Mul":
      const a$30 = e$4._0;
      const b$27 = e$4._1;
      return 1 + max(depth(a$30), depth(b$27));
    case "Div":
      const a$31 = e$4._0;
      const b$28 = e$4._1;
      return 1 + max(depth(a$31), depth(b$28));
  }
};
const simplify = e$5 => {
  switch (e$5.TAG) {
    case "Add":
      const a$32 = e$5._0;
      const b$29 = e$5._1;
      const $s12 = [simplify(a$32), simplify(b$29)];
      const $dt0 = () => {
  const sb$1 = $s12[1];
  const sa$2 = $s12[0];
  return Add(sa$2, sb$1);
};
      if ($s12[0].TAG === "Lit") {
        if ($s12[0]._0 === 0) {
          const sb = $s12[1];
          return sb;
        } else {
          if ($s12[1].TAG === "Lit") {
            if ($s12[1]._0 === 0) {
              const sa$1 = $s12[0];
              return sa$1;
            } else {
              const y$5 = $s12[1]._0;
              const x$13 = $s12[0]._0;
              return Lit(x$13 + y$5);
            }
          } else {
            return $dt0();
          }
        }
      } else {
        if ($s12[1].TAG === "Lit") {
          if ($s12[1]._0 === 0) {
            const sa = $s12[0];
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
      const $s13 = [simplify(a$33), simplify(b$30)];
      const $dt0$1 = () => Lit(0);
      const $dt1 = () => {
  const sb$5 = $s13[1];
  const sa$5 = $s13[0];
  return Mul(sa$5, sb$5);
};
      if ($s13[0].TAG === "Lit") {
        switch ($s13[0]._0) {
          case 0:
            return Lit(0);
          case 1:
            if ($s13[1].TAG === "Lit") {
              switch ($s13[1]._0) {
                case 0:
                  return $dt0$1();
                case 1:
                  const sb$4 = $s13[1];
                  return sb$4;
                default:
                  const sb$3 = $s13[1];
                  return sb$3;
              }
            } else {
              const sb$2 = $s13[1];
              return sb$2;
            }
          default:
            if ($s13[1].TAG === "Lit") {
              switch ($s13[1]._0) {
                case 0:
                  return $dt0$1();
                case 1:
                  const sa$4 = $s13[0];
                  return sa$4;
                default:
                  const y$6 = $s13[1]._0;
                  const x$14 = $s13[0]._0;
                  return Lit(x$14 * y$6);
              }
            } else {
              return $dt1();
            }
        }
      } else {
        if ($s13[1].TAG === "Lit") {
          switch ($s13[1]._0) {
            case 0:
              return $dt0$1();
            case 1:
              const sa$3 = $s13[0];
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
      const $s14 = [simplify(a$34), simplify(b$31)];
      if ($s14[1].TAG === "Lit") {
        if ($s14[1]._0 === 0) {
          const sa$8 = $s14[0];
          return sa$8;
        } else {
          if ($s14[0].TAG === "Lit") {
            const y$7 = $s14[1]._0;
            const x$15 = $s14[0]._0;
            return Lit(x$15 - y$7);
          } else {
            const sa$7 = $s14[0];
            const sb$7 = $s14[1];
            return Sub(sa$7, sb$7);
          }
        }
      } else {
        const sb$6 = $s14[1];
        const sa$6 = $s14[0];
        return Sub(sa$6, sb$6);
      }
    case "Neg":
      const a$35 = e$5._0;
      const $s15 = simplify(a$35);
      if ($s15.TAG === "Lit") {
        const x$16 = $s15._0;
        return Lit(neg(x$16));
      } else {
        const sa$9 = $s15;
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
const mapRes = (f$11, r$1) => {
  switch (r$1.TAG) {
    case "Ok":
      const a$37 = r$1._0;
      return Ok(Dartea_runtime.$$curry(f$11, [a$37]));
    case "Err":
      const e$6 = r$1._0;
      return Err(e$6);
  }
};
const andThenRes = (f$12, r$2) => {
  switch (r$2.TAG) {
    case "Ok":
      const a$38 = r$2._0;
      return f$12(a$38);
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
const maybeToRes = (e$8, m$11) => {
  if (typeof m$11 === "object") {
    const a$40 = m$11._0;
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
      return andThenRes(x$17 => mapRes(y$8 => x$17 + y$8, evalR(b$33)), evalR(a$42));
    case "Sub":
      const a$43 = e$9._0;
      const b$34 = e$9._1;
      return andThenRes(x$18 => mapRes(y$9 => x$18 - y$9, evalR(b$34)), evalR(a$43));
    case "Mul":
      const a$44 = e$9._0;
      const b$35 = e$9._1;
      return andThenRes(x$19 => mapRes(y$10 => x$19 * y$10, evalR(b$35)), evalR(a$44));
    case "Div":
      const a$45 = e$9._0;
      const b$36 = e$9._1;
      return andThenRes(y$11 => {
  if (y$11 === 0) {
    return Err("div by zero in " + show(e$9));
  } else {
    return mapRes(x$20 => (x$20 / y$11) | 0, evalR(a$45));
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
const vec = (x$21, y$12) => Tuple.pair(x$21, y$12);
const vx = v$7 => Tuple.first(v$7);
const vy = v$8 => Tuple.second(v$8);
const vadd = (a$46, b$37) => {
  const x$22 = Tuple.first(a$46) + Tuple.first(b$37);
  const y$13 = Tuple.second(a$46) + Tuple.second(b$37);
  return Tuple.pair(x$22, y$13);
};
const vsub = (a$47, b$38) => {
  const x$23 = Tuple.first(a$47) - Tuple.first(b$38);
  const y$14 = Tuple.second(a$47) - Tuple.second(b$38);
  return Tuple.pair(x$23, y$14);
};
const vscale = (k, v$9) => {
  const x$24 = k * vx(v$9);
  const y$15 = k * vy(v$9);
  return Tuple.pair(x$24, y$15);
};
const vdot = (a$48, b$39) => (Tuple.first(a$48) * Tuple.first(b$39)) + (Tuple.second(a$48) * Tuple.second(b$39));
const vlen2 = v$10 => vdot(v$10, v$10);
const manhattan = (a$49, b$40) => {
  const x$25 = Tuple.first(a$49) - Tuple.first(b$40);
  const x$26 = Tuple.second(a$49) - Tuple.second(b$40);
  return ((x$25 < 0) ? neg(x$25) : x$25) + ((x$26 < 0) ? neg(x$26) : x$26);
};
const vshow = v$11 => "(" + ($$String.fromInt(vx(v$11)) + (", " + ($$String.fromInt(vy(v$11)) + ")")));
const swap = t => Tuple.pair(Tuple.second(t), Tuple.first(t));
const nil = ($p0, z) => z;
const cons = (x$27, xs, f$13, z$1) => Dartea_runtime.$$curry(f$13, [x$27, Dartea_runtime.$$curry(xs, [f$13, z$1])]);
const foldr = (f$14, z$2, xs$1) => Dartea_runtime.$$curry(xs$1, [f$14, z$2]);
const lmap = (f$15, xs$2, g$2, z$3) => Dartea_runtime.$$curry(xs$2, [(x$28, acc$5) => Dartea_runtime.$$curry(g$2, [Dartea_runtime.$$curry(f$15, [x$28]), acc$5]), z$3]);
const lfilter = (p$1, xs$3, g$3, z$4) => Dartea_runtime.$$curry(xs$3, [(x$29, acc$6) => {
  if (p$1(x$29)) {
    return Dartea_runtime.$$curry(g$3, [x$29, acc$6]);
  } else {
    return acc$6;
  }
}, z$4]);
const lsum = xs$4 => foldr((x$30, acc$7) => x$30 + acc$7, 0, xs$4);
const llen = xs$5 => foldr(($p0$1, acc$8) => acc$8 + 1, 0, xs$5);
const lall = (p$2, xs$6) => foldr((x$31, acc$9) => p$2(x$31) && acc$9, true, xs$6);
const lany = (p$3, xs$7) => foldr((x$32, acc$10) => p$3(x$32) || acc$10, false, xs$7);
const lrange = (lo$2, hi$2, eta1$2, eta2) => Dartea_runtime.$$curry((lo$2 > hi$2) ? nil : ($s18, $s19) => cons(lo$2, ($s16, $s17) => lrange(lo$2 + 1, hi$2, $s16, $s17), $s18, $s19), [eta1$2, eta2]);
const ljoin = (sep, xs$8) => foldr((x$33, acc$11) => {
  if (acc$11 === "") {
    return x$33;
  } else {
    return x$33 + (sep + acc$11);
  }
}, "", xs$8);
const lshow = xs$9 => "[" + (ljoin(", ", ($s20, $s21) => lmap($$String.fromInt, xs$9, $s20, $s21)) + "]");
const primesTo = (n$19, eta1$3, eta2$1) => lfilter(isPrime, ($s22, $s23) => lrange(2, n$19, $s22, $s23), eta1$3, eta2$1);
const squares = (n$20, eta1$4, eta2$2) => lmap(x$34 => x$34 * x$34, ($s24, $s25) => lrange(1, n$20, $s24, $s25), eta1$4, eta2$2);
const fizzbuzz = n$21 => {
  if (divides(15, n$21)) {
    return "FizzBuzz";
  } else {
    if (divides(3, n$21)) {
      return "Fizz";
    } else {
      if (divides(5, n$21)) {
        return "Buzz";
      } else {
        return $$String.fromInt(n$21);
      }
    }
  }
};
const fizzbuzzTo = n$22 => ljoin(" ", ($s28, $s29) => lmap(fizzbuzz, ($s26, $s27) => lrange(1, n$22, $s26, $s27), $s28, $s29));
const polyTest = Tuple.pair(Tuple.pair(Basics.identity(1), Basics.identity("str")), Tuple.pair(Basics.always(true, "x"), Basics.always("y", 42)));
const line = (label, value) => label + (" = " + value);
const value$1 = $$String.fromInt(fib(25));
const value$2 = $$String.fromInt(fact(10));
const value$3 = $$String.fromInt(gcd(462, 1071));
const value$4 = $$String.fromInt(powFast(2, 16));
const value$5 = $$String.fromInt(collatzSteps(27));
const value$6 = $$String.fromInt(countPrimes(100));
const value$7 = $$String.fromInt(ack(2, 3));
const value$8 = $$String.fromInt(sumDigits(987654));
const value$9 = $$String.fromInt(reverseInt(12321));
const report = ljoin("; ", ($s46, $s47) => cons("fib 25" + (" = " + value$1), ($s44, $s45) => cons("fact 10" + (" = " + value$2), ($s42, $s43) => cons("gcd 462 1071" + (" = " + value$3), ($s40, $s41) => cons("2^16" + (" = " + value$4), ($s38, $s39) => cons("collatz 27" + (" = " + value$5), ($s36, $s37) => cons("primes<100" + (" = " + value$6), ($s34, $s35) => cons("ack 2 3" + (" = " + value$7), ($s32, $s33) => cons("sumDigits 987654" + (" = " + value$8), ($s30, $s31) => cons("palindrome 12321" + (" = " + value$9), nil, $s30, $s31), $s32, $s33), $s34, $s35), $s36, $s37), $s38, $s39), $s40, $s41), $s42, $s43), $s44, $s45), $s46, $s47));
const m$12 = $$eval(expr1);
let $s48;
if (typeof m$12 === "object") {
  const v$12 = m$12._0;
  $s48 = v$12;
} else {
  $s48 = 0;
}
const demoExpr1 = show(simplify(expr1)) + (" -> " + $$String.fromInt($s48));
const demoExpr2 = showRes(evalR(expr2));
const demoExpr3 = show(simplify(expr3));
const demoVec = vshow(vadd(Tuple.pair(3, 4), vscale(2, Tuple.pair(1, 1))));
const demoList = lshow(($s49, $s50) => squares(10, $s49, $s50)) + (" sum=" + $$String.fromInt(lsum(($s51, $s52) => squares(10, $s51, $s52))));
const demoPrimes = lshow(($s53, $s54) => primesTo(50, $s53, $s54));
const demoFizz = fizzbuzzTo(20);
const m$13 = addStrings("40", "2");
let $s55;
if (typeof m$13 === "object") {
  const v$13 = m$13._0;
  $s55 = v$13;
} else {
  $s55 = 0;
}
const demoMaybe = $s55;
const m$14 = mapMaybe(x$35 => x$35 * 6, $$String.toInt("7"));
let $s56;
if (typeof m$14 === "object") {
  const v$14 = m$14._0;
  $s56 = v$14;
} else {
  $s56 = 0;
}
const demoChain = $$String.fromInt($s56);
export { Add, Div, Err, Lit, Mul, Neg, Ok, Sub, abs, ack, addStrings, andThen, andThenRes, apply, binop, clamp, collatzSteps, compose, cons, countPrimes, demoChain, demoExpr1, demoExpr2, demoExpr3, demoFizz, demoList, demoMaybe, demoPrimes, demoVec, depth, digitCount, divides, $$eval, evalR, even, expr1, expr2, expr3, fact, fib, fizzbuzz, fizzbuzzTo, flip, foldRange, foldr, gcd, isJust, isPalindromeInt, isPrime, keepIf, lall, lany, lcm, lfilter, line, ljoin, llen, lmap, lrange, lshow, lsum, manhattan, map2, mapMaybe, mapRes, max, maybeToRes, min, neg, nil, odd, on, orElse, polyTest, powFast, primesTo, quot, rem, report, resToMaybe, reverseInt, safeDiv, show, showRes, sign, simplify, size, squares, sumDigits, sumTo, swap, thrice, twice, vadd, vdot, vec, vlen2, vscale, vshow, vsub, vx, vy, withDefault };
