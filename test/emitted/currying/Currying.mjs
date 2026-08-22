import * as Dartea_runtime from "./Dartea_runtime.mjs";
import * as Basics from "./Basics.mjs";
import * as Maybe from "./Maybe.mjs";
import * as $$String from "./String.mjs";
const Boxed = _0 => ({ _0: _0 });
const add = (a, b) => a + b;
const mul = (a$1, b$1) => a$1 * b$1;
const saturated = 3;
const inc = $s1 => add(1, $s1);
const partial = inc(5) + inc(6);
const twice = (f, n) => f(f(n));
const f$1 = $s2 => add(1, $s2);
const concreteHigherOrder = f$1(f$1(5));
const bodyIsALambda = (a$2, b$2) => a$2 - b$2;
const sharesWorkThenReturns = (n$1, eta1) => {
  const doubled = n$1 + n$1;
  return doubled + eta1;
};
const callsAtTwo = f$2 => f$2(3, 4);
const etaExpanded = -1 + sharesWorkThenReturns(3, 4);
const computedCallee = (c, f$3, g, n$2) => (c ? f$3 : g)(n$2);
const fromARecord = (handlers, n$3) => handlers.go(n$3);
const f$4 = $s3 => add(1, $s3);
const g$1 = $s4 => mul(2, $s4);
const handlers$1 = { go: $s5 => add(7, $s5) };
const notAnIdentifier = (true ? f$4 : g$1)(5) + handlers$1.go(5);
const overApplied = Basics.identity(add)(3, 4);
const f$5 = (a$3, b$3) => a$3 + b$3;
const nestedLambda = f$5(3, 4);
const apply = (f$6, x) => Dartea_runtime.$$curry(f$6, [x]);
const f$7 = $s6 => add(3, $s6);
const polymorphicParameter = f$7(4) + 7;
const unbox = b$4 => {
  const f$8 = b$4._0;
  return f$8(1, 2);
};
const keepGeneric = g$2 => Maybe.Just(g$2);
const $s7 = Maybe.Just(add);
let $s8;
if (typeof $s7 === "object") {
  const f$9 = $s7._0;
  const b$5 = Boxed(mul);
  let $s9;
  const f$10 = b$5._0;
  $s9 = f$10(1, 2);
  $s8 = f$9(1, 2) + $s9;
} else {
  $s8 = 0;
}
const throughAGenericSlot = $s8;
const line = (name, value) => name + ("=" + $$String.fromInt(value));
const report = line("saturated", 3) + ("; " + (line("partial", partial) + ("; " + (line("concreteHigherOrder", concreteHigherOrder) + ("; " + (line("etaExpanded", etaExpanded) + ("; " + (line("notAnIdentifier", notAnIdentifier) + ("; " + (line("overApplied", overApplied) + ("; " + (line("nestedLambda", nestedLambda) + ("; " + (line("polymorphicParameter", polymorphicParameter) + ("; " + line("throughAGenericSlot", throughAGenericSlot))))))))))))))));
export { report };
