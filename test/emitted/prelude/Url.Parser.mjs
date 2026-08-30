// Compiled by dartea, an independent compiler. Not affiliated with or
// endorsed by the Elm project.
// Contains material derived from elm/url,
// Copyright 2017-present Evan Czaplicki, under the BSD 3-Clause License.
// dartea's LICENSE carries the full text.
import * as Dartea_runtime from "./Dartea_runtime.mjs";
import * as Basics from "./Basics.mjs";
import * as Dict from "./Dict.mjs";
import * as List from "./List.mjs";
import * as Maybe from "./Maybe.mjs";
import * as $$String from "./String.mjs";
import * as Url from "./Url.mjs";
import * as Url$Parser$Internal from "./Url.Parser.Internal.mjs";
const Parser = _0 => ({ _0: _0 });
const slash = ($p0, $p1) => {
  const parseBefore = $p0._0;
  const parseAfter = $p1._0;
  return Parser(state => List.concatMap(parseAfter, parseBefore(state)));
};
const $$lt$slash$gt = (eta1, eta2) => slash(eta1, eta2);
const State = ($a0, $a1, $a2, $a3, $a4) => ({ visited: $a0, unvisited: $a1, params: $a2, frag: $a3, value: $a4 });
const query = $p0$1 => {
  const queryParser = $p0$1._0;
  return Parser($p0$2 => {
  const value = $p0$2.value;
  const frag = $p0$2.frag;
  const params = $p0$2.params;
  const unvisited = $p0$2.unvisited;
  const visited = $p0$2.visited;
  return { hd: State(visited, unvisited, params, frag, Dartea_runtime.$$apply1(value, Dartea_runtime.$$apply1(queryParser, params))), tl: 0 };
});
};
const $$lt$question$gt = (eta1$1, eta2$1) => slash(eta1$1, query(eta2$1));
const custom = (tipe, stringToSomething) => Parser($p0$3 => {
  const value$1 = $p0$3.value;
  const frag$1 = $p0$3.frag;
  const params$1 = $p0$3.params;
  const unvisited$1 = $p0$3.unvisited;
  const visited$1 = $p0$3.visited;
  if (unvisited$1 === 0) {
    return 0;
  } else {
    const next = unvisited$1.hd;
    const rest = unvisited$1.tl;
    const $s1 = stringToSomething(next);
    if (typeof $s1 === "object") {
      const nextValue = $s1._0;
      return { hd: State({ hd: next, tl: visited$1 }, rest, params$1, frag$1, Dartea_runtime.$$apply1(value$1, nextValue)), tl: 0 };
    } else {
      return 0;
    }
  }
});
const string = custom("STRING", Maybe.Just);
const $$int = custom("NUMBER", $$String.toInt);
const s = str => Parser($p0$4 => {
  const value$2 = $p0$4.value;
  const frag$2 = $p0$4.frag;
  const params$2 = $p0$4.params;
  const unvisited$2 = $p0$4.unvisited;
  const visited$2 = $p0$4.visited;
  if (unvisited$2 === 0) {
    return 0;
  } else {
    const next$1 = unvisited$2.hd;
    const rest$1 = unvisited$2.tl;
    if (next$1 === str) {
      return { hd: State({ hd: next$1, tl: visited$2 }, rest$1, params$2, frag$2, value$2), tl: 0 };
    } else {
      return 0;
    }
  }
});
const mapState = (func, $p1$1) => {
  const value$3 = $p1$1.value;
  const frag$3 = $p1$1.frag;
  const params$3 = $p1$1.params;
  const unvisited$3 = $p1$1.unvisited;
  const visited$3 = $p1$1.visited;
  return State(visited$3, unvisited$3, params$3, frag$3, Dartea_runtime.$$apply1(func, value$3));
};
const map = (subValue, $p1$2) => {
  const parseArg = $p1$2._0;
  return Parser($p0$5 => {
  const value$4 = $p0$5.value;
  const frag$4 = $p0$5.frag;
  const params$4 = $p0$5.params;
  const unvisited$4 = $p0$5.unvisited;
  const visited$4 = $p0$5.visited;
  return List.map($s2 => mapState(value$4, $s2), parseArg(State(visited$4, unvisited$4, params$4, frag$4, subValue)));
});
};
const oneOf = parsers => Parser(state$1 => List.concatMap($p0$6 => {
  const parser = $p0$6._0;
  return parser(state$1);
}, parsers));
const top = Parser(state$2 => ({ hd: state$2, tl: 0 }));
const fragment = toFrag => Parser($p0$7 => {
  const value$5 = $p0$7.value;
  const frag$5 = $p0$7.frag;
  const params$5 = $p0$7.params;
  const unvisited$5 = $p0$7.unvisited;
  const visited$5 = $p0$7.visited;
  return { hd: State(visited$5, unvisited$5, params$5, frag$5, Dartea_runtime.$$apply1(value$5, Dartea_runtime.$$apply1(toFrag, frag$5))), tl: 0 };
});
const getFirstMatch = states => {
  if (states === 0) {
    return Maybe.Nothing;
  } else {
    const state$3 = states.hd;
    const rest$2 = states.tl;
    const $s3 = state$3.unvisited;
    const $dt0 = () => getFirstMatch(rest$2);
    if ($s3 === 0) {
      return Maybe.Just(state$3.value);
    } else {
      if ($s3.hd === "") {
        if ($s3.tl === 0) {
          return Maybe.Just(state$3.value);
        } else {
          return $dt0();
        }
      } else {
        return $dt0();
      }
    }
  }
};
const removeFinalEmpty = segments => {
  if (segments === 0) {
    return 0;
  } else {
    if (segments.hd === "") {
      if (segments.tl === 0) {
        return 0;
      } else {
        const rest$4 = segments.tl;
        const segment$1 = segments.hd;
        return { hd: segment$1, tl: removeFinalEmpty(rest$4) };
      }
    } else {
      const segment = segments.hd;
      const rest$3 = segments.tl;
      return { hd: segment, tl: removeFinalEmpty(rest$3) };
    }
  }
};
const preparePath = path => {
  const $s4 = $$String.split("/", path);
  const $dt0$1 = () => {
  const segments$2 = $s4;
  return removeFinalEmpty(segments$2);
};
  if ($s4 !== 0) {
    if ($s4.hd === "") {
      const segments$1 = $s4.tl;
      return removeFinalEmpty(segments$1);
    } else {
      return $dt0$1();
    }
  } else {
    return $dt0$1();
  }
};
const addToParametersHelp = (value$6, maybeList) => {
  if (maybeList === "Nothing") {
    return Maybe.Just({ hd: value$6, tl: 0 });
  } else {
    const list = maybeList._0;
    return Maybe.Just({ hd: value$6, tl: list });
  }
};
const addParam = (segment$2, dict) => {
  const $s5 = $$String.split("=", segment$2);
  if ($s5 !== 0) {
    if ($s5.tl !== 0) {
      if ($s5.tl.tl === 0) {
        const rawValue = $s5.tl.hd;
        const rawKey = $s5.hd;
        const $s6 = Url.percentDecode(rawKey);
        if ($s6 === "Nothing") {
          return dict;
        } else {
          const key = $s6._0;
          const $s7 = Url.percentDecode(rawValue);
          if ($s7 === "Nothing") {
            return dict;
          } else {
            const value$7 = $s7._0;
            return Dict.update(key, $s8 => addToParametersHelp(value$7, $s8), dict);
          }
        }
      } else {
        return dict;
      }
    } else {
      return dict;
    }
  } else {
    return dict;
  }
};
const prepareQuery = maybeQuery => {
  if (maybeQuery === "Nothing") {
    return Dict.empty;
  } else {
    const qry = maybeQuery._0;
    return List.foldr(addParam, Dict.empty, $$String.split("&", qry));
  }
};
const parse = ($p0$8, url) => {
  const parser$1 = $p0$8._0;
  return getFirstMatch(parser$1(State(0, preparePath(url.path), prepareQuery(url.query), url.fragment, Basics.identity)));
};
export { $$lt$slash$gt, $$lt$question$gt, custom, fragment, $$int, map, oneOf, parse, query, s, string, top };
