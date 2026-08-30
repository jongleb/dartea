// Compiled by dartea, an independent compiler. Not affiliated with or
// endorsed by the Elm project.
import * as Dartea_runtime from "./Dartea_runtime.mjs";
import * as Dartea_browser from "./Dartea_browser.mjs";
import * as List from "./List.mjs";
const succeed = Dartea_browser.$$Task$succeed;
const fail = Dartea_browser.$$Task$fail;
const andThen = Dartea_browser.$$Task$andThen;
const onError = Dartea_browser.$$Task$onError;
const map = (func, task) => Dartea_browser.$$Task$andThen(a => Dartea_browser.$$Task$succeed(Dartea_runtime.$$apply1(func, a)), task);
const map2 = (func$1, one, other) => Dartea_browser.$$Task$andThen(a$1 => Dartea_browser.$$Task$andThen(b => Dartea_browser.$$Task$succeed(Dartea_runtime.$$apply2(func$1, a$1, b)), other), one);
const map3 = (func$2, one$1, other$1, third) => Dartea_browser.$$Task$andThen(a$2 => map2(Dartea_runtime.$$apply1(func$2, a$2), other$1, third), one$1);
const map4 = (func$3, one$2, other$2, third$1, fourth) => Dartea_browser.$$Task$andThen(a$3 => map3(Dartea_runtime.$$apply1(func$3, a$3), other$2, third$1, fourth), one$2);
const map5 = (func$4, one$3, other$3, third$2, fourth$1, fifth) => Dartea_browser.$$Task$andThen(a$4 => map4(Dartea_runtime.$$apply1(func$4, a$4), other$3, third$2, fourth$1, fifth), one$3);
const mapError = (convert, task$1) => Dartea_browser.$$Task$onError(reason => Dartea_browser.$$Task$fail(Dartea_runtime.$$apply1(convert, reason)), task$1);
const sequence = tasks => List.foldr(($s1, $s2) => map2((value, gathered) => ({ hd: value, tl: gathered }), $s1, $s2), Dartea_browser.$$Task$succeed(0), tasks);
const perform = Dartea_browser.$$Task$perform;
const attempt = Dartea_browser.$$Task$attempt;
export { andThen, attempt, fail, map, map2, map3, map4, map5, mapError, onError, perform, sequence, succeed };
