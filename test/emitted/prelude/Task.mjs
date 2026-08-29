// Compiled by dartea, an independent compiler. Not affiliated with or
// endorsed by the Elm project.
// Contains material derived from elm/core,
// Copyright 2014-present Evan Czaplicki, under the BSD 3-Clause License.
// dartea's LICENSE carries the full text.
import * as Dartea_runtime from "./Dartea_runtime.mjs";
import * as Dartea_browser from "./Dartea_browser.mjs";
import * as List from "./List.mjs";
const succeed = Dartea_browser.$$Task$succeed;
const fail = Dartea_browser.$$Task$fail;
const andThen = Dartea_browser.$$Task$andThen;
const onError = Dartea_browser.$$Task$onError;
const map = (func, task) => andThen(a => succeed(Dartea_runtime.$$curry(func, [a])), task);
const map2 = (func$1, one, other) => andThen(a$1 => andThen(b => succeed(Dartea_runtime.$$curry(func$1, [a$1, b])), other), one);
const map3 = (func$2, one$1, other$1, third) => andThen(a$2 => map2(Dartea_runtime.$$curry(func$2, [a$2]), other$1, third), one$1);
const map4 = (func$3, one$2, other$2, third$1, fourth) => andThen(a$3 => map3(Dartea_runtime.$$curry(func$3, [a$3]), other$2, third$1, fourth), one$2);
const map5 = (func$4, one$3, other$3, third$2, fourth$1, fifth) => andThen(a$4 => map4(Dartea_runtime.$$curry(func$4, [a$4]), other$3, third$2, fourth$1, fifth), one$3);
const mapError = (convert, task$1) => onError(reason => fail(Dartea_runtime.$$curry(convert, [reason])), task$1);
const sequence = tasks => List.foldr(($s1, $s2) => map2((value, gathered) => ({ hd: value, tl: gathered }), $s1, $s2), succeed(0), tasks);
const perform = Dartea_browser.$$Task$perform;
const attempt = Dartea_browser.$$Task$attempt;
export { andThen, attempt, fail, map, map2, map3, map4, map5, mapError, onError, perform, sequence, succeed };
