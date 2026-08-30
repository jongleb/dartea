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
const notAnIdentifier = (true ? $s3 => add(1, $s3) : $s4 => mul(2, $s4))(5) + { go: $s5 => add(7, $s5) }.go(5);
const overApplied = Basics.identity(add)(3, 4);
const $s6 = Maybe.Just(add);
let $s7;
if (typeof $s6 === "object") {
  const f$1 = $s6._0;
  const $s8 = Boxed(mul);
  let $s9;
  const f$2 = $s8._0;
  $s9 = f$2(1, 2);
  $s7 = f$1(1, 2) + $s9;
} else {
  $s7 = 0;
}
const throughAGenericSlot = $s7;
const line = (name, value) => name + ("=" + $$String.fromInt(value));
const report = line("saturated", 3) + ("; " + (line("partial", partial) + ("; " + (line("concreteHigherOrder", concreteHigherOrder) + ("; " + (line("etaExpanded", etaExpanded) + ("; " + (line("notAnIdentifier", notAnIdentifier) + ("; " + (line("overApplied", overApplied) + ("; " + (line("nestedLambda", 7) + ("; " + (line("polymorphicParameter", 14) + ("; " + line("throughAGenericSlot", throughAGenericSlot))))))))))))))));
export { report };
