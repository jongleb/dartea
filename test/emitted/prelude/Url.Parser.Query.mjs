import * as Dartea_runtime from "./Dartea_runtime.mjs";
import * as Dict from "./Dict.mjs";
import * as Maybe from "./Maybe.mjs";
import * as $$String from "./String.mjs";
import * as Url$Parser$Internal from "./Url.Parser.Internal.mjs";
const custom = (key, func) => Url$Parser$Internal.Parser(dict => Dartea_runtime.$$apply1(func, Maybe.withDefault(0, Dict.get(key, dict))));
const string = key$1 => custom(key$1, stringList => {
  if (stringList !== 0) {
    if (stringList.tl === 0) {
      const str = stringList.hd;
      return Maybe.Just(str);
    } else {
      return Maybe.Nothing;
    }
  } else {
    return Maybe.Nothing;
  }
});
const $$int = key$2 => custom(key$2, stringList$1 => {
  if (stringList$1 !== 0) {
    if (stringList$1.tl === 0) {
      const str$1 = stringList$1.hd;
      return $$String.toInt(str$1);
    } else {
      return Maybe.Nothing;
    }
  } else {
    return Maybe.Nothing;
  }
});
const $$enum = (key$3, dict$1) => custom(key$3, stringList$2 => {
  if (stringList$2 !== 0) {
    if (stringList$2.tl === 0) {
      const str$2 = stringList$2.hd;
      return Dict.get(str$2, dict$1);
    } else {
      return Maybe.Nothing;
    }
  } else {
    return Maybe.Nothing;
  }
});
const map = (func$1, $p1) => {
  const a = $p1._0;
  return Url$Parser$Internal.Parser(dict$2 => Dartea_runtime.$$apply1(func$1, Dartea_runtime.$$apply1(a, dict$2)));
};
const map2 = (func$2, $p1$1, $p2) => {
  const a$1 = $p1$1._0;
  const b = $p2._0;
  return Url$Parser$Internal.Parser(dict$3 => Dartea_runtime.$$apply2(func$2, Dartea_runtime.$$apply1(a$1, dict$3), Dartea_runtime.$$apply1(b, dict$3)));
};
const map3 = (func$3, $p1$2, $p2$1, $p3) => {
  const a$2 = $p1$2._0;
  const b$1 = $p2$1._0;
  const c = $p3._0;
  return Url$Parser$Internal.Parser(dict$4 => Dartea_runtime.$$curry(func$3, [Dartea_runtime.$$apply1(a$2, dict$4), Dartea_runtime.$$apply1(b$1, dict$4), Dartea_runtime.$$apply1(c, dict$4)]));
};
const map4 = (func$4, $p1$3, $p2$2, $p3$1, $p4) => {
  const a$3 = $p1$3._0;
  const b$2 = $p2$2._0;
  const c$1 = $p3$1._0;
  const d = $p4._0;
  return Url$Parser$Internal.Parser(dict$5 => Dartea_runtime.$$curry(func$4, [Dartea_runtime.$$apply1(a$3, dict$5), Dartea_runtime.$$apply1(b$2, dict$5), Dartea_runtime.$$apply1(c$1, dict$5), Dartea_runtime.$$apply1(d, dict$5)]));
};
const map5 = (func$5, $p1$4, $p2$3, $p3$2, $p4$1, $p5) => {
  const a$4 = $p1$4._0;
  const b$3 = $p2$3._0;
  const c$2 = $p3$2._0;
  const d$1 = $p4$1._0;
  const e = $p5._0;
  return Url$Parser$Internal.Parser(dict$6 => Dartea_runtime.$$curry(func$5, [Dartea_runtime.$$apply1(a$4, dict$6), Dartea_runtime.$$apply1(b$3, dict$6), Dartea_runtime.$$apply1(c$2, dict$6), Dartea_runtime.$$apply1(d$1, dict$6), Dartea_runtime.$$apply1(e, dict$6)]));
};
const map6 = (func$6, $p1$5, $p2$4, $p3$3, $p4$2, $p5$1, $p6) => {
  const a$5 = $p1$5._0;
  const b$4 = $p2$4._0;
  const c$3 = $p3$3._0;
  const d$2 = $p4$2._0;
  const e$1 = $p5$1._0;
  const f = $p6._0;
  return Url$Parser$Internal.Parser(dict$7 => Dartea_runtime.$$curry(func$6, [Dartea_runtime.$$apply1(a$5, dict$7), Dartea_runtime.$$apply1(b$4, dict$7), Dartea_runtime.$$apply1(c$3, dict$7), Dartea_runtime.$$apply1(d$2, dict$7), Dartea_runtime.$$apply1(e$1, dict$7), Dartea_runtime.$$apply1(f, dict$7)]));
};
const map7 = (func$7, $p1$6, $p2$5, $p3$4, $p4$3, $p5$2, $p6$1, $p7) => {
  const a$6 = $p1$6._0;
  const b$5 = $p2$5._0;
  const c$4 = $p3$4._0;
  const d$3 = $p4$3._0;
  const e$2 = $p5$2._0;
  const f$1 = $p6$1._0;
  const g = $p7._0;
  return Url$Parser$Internal.Parser(dict$8 => Dartea_runtime.$$curry(func$7, [Dartea_runtime.$$apply1(a$6, dict$8), Dartea_runtime.$$apply1(b$5, dict$8), Dartea_runtime.$$apply1(c$4, dict$8), Dartea_runtime.$$apply1(d$3, dict$8), Dartea_runtime.$$apply1(e$2, dict$8), Dartea_runtime.$$apply1(f$1, dict$8), Dartea_runtime.$$apply1(g, dict$8)]));
};
const map8 = (func$8, $p1$7, $p2$6, $p3$5, $p4$4, $p5$3, $p6$2, $p7$1, $p8) => {
  const a$7 = $p1$7._0;
  const b$6 = $p2$6._0;
  const c$5 = $p3$5._0;
  const d$4 = $p4$4._0;
  const e$3 = $p5$3._0;
  const f$2 = $p6$2._0;
  const g$1 = $p7$1._0;
  const h = $p8._0;
  return Url$Parser$Internal.Parser(dict$9 => Dartea_runtime.$$curry(func$8, [Dartea_runtime.$$apply1(a$7, dict$9), Dartea_runtime.$$apply1(b$6, dict$9), Dartea_runtime.$$apply1(c$5, dict$9), Dartea_runtime.$$apply1(d$4, dict$9), Dartea_runtime.$$apply1(e$3, dict$9), Dartea_runtime.$$apply1(f$2, dict$9), Dartea_runtime.$$apply1(g$1, dict$9), Dartea_runtime.$$apply1(h, dict$9)]));
};
export { custom, $$enum, $$int, map, map2, map3, map4, map5, map6, map7, map8, string };
