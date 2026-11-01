import * as Dartea_runtime from "./Dartea_runtime.mjs";
import * as Dict from "./Dict.mjs";
import * as List from "./List.mjs";
const Set_elm_builtin = _0 => ({ _0: _0 });
const empty = Set_elm_builtin(Dict.empty);
const singleton = key => Set_elm_builtin(Dict.singleton(key, null));
const insert = (key$1, $p1) => {
  const dict = $p1._0;
  return Set_elm_builtin(Dict.insert(key$1, null, dict));
};
const remove = (key$2, $p1$1) => {
  const dict$1 = $p1$1._0;
  return Set_elm_builtin(Dict.remove(key$2, dict$1));
};
const isEmpty = $p0 => {
  const dict$2 = $p0._0;
  return Dict.isEmpty(dict$2);
};
const member = (key$3, $p1$2) => {
  const dict$3 = $p1$2._0;
  return Dict.member(key$3, dict$3);
};
const size = $p0$1 => {
  const dict$4 = $p0$1._0;
  return Dict.size(dict$4);
};
const union = ($p0$2, $p1$3) => {
  const dict1 = $p0$2._0;
  const dict2 = $p1$3._0;
  return Set_elm_builtin(Dict.union(dict1, dict2));
};
const intersect = ($p0$3, $p1$4) => {
  const dict1$1 = $p0$3._0;
  const dict2$1 = $p1$4._0;
  return Set_elm_builtin(Dict.intersect(dict1$1, dict2$1));
};
const diff = ($p0$4, $p1$5) => {
  const dict1$2 = $p0$4._0;
  const dict2$2 = $p1$5._0;
  return Set_elm_builtin(Dict.diff(dict1$2, dict2$2));
};
const toList = $p0$5 => {
  const dict$5 = $p0$5._0;
  return Dict.keys(dict$5);
};
const fromList = list => List.foldl(insert, empty, list);
const foldl = (func, initialState, $p2) => {
  const dict$6 = $p2._0;
  return Dict.foldl((key$4, $p1$6, state) => Dartea_runtime.$$apply2(func, key$4, state), initialState, dict$6);
};
const foldr = (func$1, initialState$1, $p2$1) => {
  const dict$7 = $p2$1._0;
  return Dict.foldr((key$5, $p1$7, state$1) => Dartea_runtime.$$apply2(func$1, key$5, state$1), initialState$1, dict$7);
};
const map = (func$2, set) => List.foldl(insert, empty, foldl((x, xs) => ({ hd: Dartea_runtime.$$apply1(func$2, x), tl: xs }), 0, set));
const filter = (isGood, $p1$8) => {
  const dict$8 = $p1$8._0;
  return Set_elm_builtin(Dict.filter((key$6, $p1$9) => isGood(key$6), dict$8));
};
const partition = (isGood$1, $p1$10) => {
  const dict$9 = $p1$10._0;
  const $s1 = Dict.partition((key$7, $p1$11) => isGood$1(key$7), dict$9);
  const dict1$3 = $s1[0];
  const dict2$3 = $s1[1];
  return [Set_elm_builtin(dict1$3), Set_elm_builtin(dict2$3)];
};
export { diff, empty, filter, foldl, foldr, fromList, insert, intersect, isEmpty, map, member, partition, remove, singleton, size, toList, union };
