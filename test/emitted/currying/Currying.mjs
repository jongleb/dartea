import * as Basics from "./Basics.mjs";
import * as Maybe from "./Maybe.mjs";
import * as $$String from "./String.mjs";
const Boxed = _0 => ({ _0: _0 });
const add = (a, b) => a + b;
const mul = (a$1, b$1) => a$1 * b$1;
const inc = $s1 => add(1, $s1);
const partial = inc(5) + inc(6);
const f = $s2 => add(1, $s2);
const concreteHigherOrder = f(f(5));
const sharesWorkThenReturns = (n, eta1) => {
  const doubled = n + n;
  return doubled + eta1;
};
const etaExpanded = -1 + sharesWorkThenReturns(3, 4);
const f$1 = $s3 => add(1, $s3);
const g = $s4 => mul(2, $s4);
const handlers = { go: $s5 => add(7, $s5) };
const notAnIdentifier = (true ? f$1 : g)(5) + handlers.go(5);
const overApplied = Basics.identity(add)(3, 4);
const f$2 = (a$2, b$2) => a$2 + b$2;
const nestedLambda = f$2(3, 4);
const f$3 = $s6 => add(3, $s6);
const polymorphicParameter = f$3(4) + 7;
const $s7 = Maybe.Just(add);
let $s8;
if (typeof $s7 === "object") {
  const f$4 = $s7._0;
  const b$3 = Boxed(mul);
  let $s9;
  const f$5 = b$3._0;
  $s9 = f$5(1, 2);
  $s8 = f$4(1, 2) + $s9;
} else {
  $s8 = 0;
}
const throughAGenericSlot = $s8;
const line = (name, value) => name + ("=" + $$String.fromInt(value));
const report = line("saturated", 3) + ("; " + (line("partial", partial) + ("; " + (line("concreteHigherOrder", concreteHigherOrder) + ("; " + (line("etaExpanded", etaExpanded) + ("; " + (line("notAnIdentifier", notAnIdentifier) + ("; " + (line("overApplied", overApplied) + ("; " + (line("nestedLambda", nestedLambda) + ("; " + (line("polymorphicParameter", polymorphicParameter) + ("; " + line("throughAGenericSlot", throughAGenericSlot))))))))))))))));
export { report };
