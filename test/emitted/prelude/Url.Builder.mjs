// Compiled by dartea, an independent compiler. Not affiliated with or
// endorsed by the Elm project.
// Contains material derived from elm/url,
// Copyright 2017-present Evan Czaplicki, under the BSD 3-Clause License.
// dartea's LICENSE carries the full text.
import * as List from "./List.mjs";
import * as Maybe from "./Maybe.mjs";
import * as $$String from "./String.mjs";
import * as Url from "./Url.mjs";
const Absolute = "Absolute";
const Relative = "Relative";
const CrossOrigin = _0 => ({ _0: _0 });
const QueryParameter = (_0, _1) => ({ _0: _0, _1: _1 });
const toQueryPair = $p0 => {
  const key = $p0._0;
  const value = $p0._1;
  return key + ("=" + value);
};
const toQuery = parameters => {
  if (parameters === 0) {
    return "";
  } else {
    return "?" + $$String.join("&", List.map(toQueryPair, parameters));
  }
};
const absolute = (pathSegments, parameters$1) => "/" + ($$String.join("/", pathSegments) + toQuery(parameters$1));
const relative = (pathSegments$1, parameters$2) => $$String.join("/", pathSegments$1) + toQuery(parameters$2);
const crossOrigin = (prePath, pathSegments$2, parameters$3) => prePath + ("/" + ($$String.join("/", pathSegments$2) + toQuery(parameters$3)));
const rootToPrePath = root => {
  if (root === "Absolute") {
    return "/";
  } else {
    if (root === "Relative") {
      return "";
    } else {
      const prePath$1 = root._0;
      return prePath$1 + "/";
    }
  }
};
const custom = (root$1, pathSegments$3, parameters$4, maybeFragment) => {
  const fragmentless = rootToPrePath(root$1) + ($$String.join("/", pathSegments$3) + toQuery(parameters$4));
  if (maybeFragment === "Nothing") {
    return fragmentless;
  } else {
    const fragment = maybeFragment._0;
    return fragmentless + ("#" + fragment);
  }
};
const string = (key$1, value$1) => QueryParameter(Url.percentEncode(key$1), Url.percentEncode(value$1));
const $$int = (key$2, value$2) => QueryParameter(Url.percentEncode(key$2), $$String.fromInt(value$2));
export { Absolute, CrossOrigin, Relative, absolute, crossOrigin, custom, $$int, relative, string, toQuery };
