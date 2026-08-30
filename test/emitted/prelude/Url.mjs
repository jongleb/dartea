import * as Dartea_browser from "./Dartea_browser.mjs";
import * as Maybe from "./Maybe.mjs";
import * as $$String from "./String.mjs";
const Http = "Http";
const Https = "Https";
const Url = ($a0, $a1, $a2, $a3, $a4, $a5) => ({ protocol: $a0, host: $a1, port_: $a2, path: $a3, query: $a4, fragment: $a5 });
const chompBeforePath = (protocol, path, params, frag, str) => {
  if ($$String.isEmpty(str) || $$String.contains("@", str)) {
    return Maybe.Nothing;
  } else {
    const $s1 = $$String.indexes(":", str);
    if ($s1 === 0) {
      return Maybe.Just(Url(protocol, str, Maybe.Nothing, path, params, frag));
    } else {
      if ($s1.tl === 0) {
        const i = $s1.hd;
        const $s2 = $$String.toInt($$String.dropLeft(i + 1, str));
        if ($s2 === "Nothing") {
          return Maybe.Nothing;
        } else {
          const port_ = $s2;
          return Maybe.Just(Url(protocol, $$String.left(i, str), port_, path, params, frag));
        }
      } else {
        return Maybe.Nothing;
      }
    }
  }
};
const chompBeforeQuery = (protocol$1, params$1, frag$1, str$1) => {
  if ($$String.isEmpty(str$1)) {
    return Maybe.Nothing;
  } else {
    const $s3 = $$String.indexes("/", str$1);
    if ($s3 === 0) {
      return chompBeforePath(protocol$1, "/", params$1, frag$1, str$1);
    } else {
      const i$1 = $s3.hd;
      return chompBeforePath(protocol$1, $$String.dropLeft(i$1, str$1), params$1, frag$1, $$String.left(i$1, str$1));
    }
  }
};
const chompBeforeFragment = (protocol$2, frag$2, str$2) => {
  if ($$String.isEmpty(str$2)) {
    return Maybe.Nothing;
  } else {
    const $s4 = $$String.indexes("?", str$2);
    if ($s4 === 0) {
      return chompBeforeQuery(protocol$2, Maybe.Nothing, frag$2, str$2);
    } else {
      const i$2 = $s4.hd;
      return chompBeforeQuery(protocol$2, Maybe.Just($$String.dropLeft(i$2 + 1, str$2)), frag$2, $$String.left(i$2, str$2));
    }
  }
};
const chompAfterProtocol = (protocol$3, str$3) => {
  if ($$String.isEmpty(str$3)) {
    return Maybe.Nothing;
  } else {
    const $s5 = $$String.indexes("#", str$3);
    if ($s5 === 0) {
      return chompBeforeFragment(protocol$3, Maybe.Nothing, str$3);
    } else {
      const i$3 = $s5.hd;
      return chompBeforeFragment(protocol$3, Maybe.Just($$String.dropLeft(i$3 + 1, str$3)), $$String.left(i$3, str$3));
    }
  }
};
const fromString = str$4 => {
  if ($$String.startsWith("http://", str$4)) {
    return chompAfterProtocol(Http, $$String.dropLeft(7, str$4));
  } else {
    if ($$String.startsWith("https://", str$4)) {
      return chompAfterProtocol(Https, $$String.dropLeft(8, str$4));
    } else {
      return Maybe.Nothing;
    }
  }
};
const addPort = (maybePort, starter) => {
  if (maybePort === "Nothing") {
    return starter;
  } else {
    const port_$1 = maybePort._0;
    return starter + (":" + $$String.fromInt(port_$1));
  }
};
const addPrefixed = (prefix, maybeSegment, starter$1) => {
  if (maybeSegment === "Nothing") {
    return starter$1;
  } else {
    const segment = maybeSegment._0;
    return starter$1 + (prefix + segment);
  }
};
const toString = url => {
  const $s6 = url.protocol;
  let $s7;
  if ($s6 === "Http") {
    $s7 = "http://";
  } else {
    $s7 = "https://";
  }
  const http = $s7;
  return addPrefixed("#", url.fragment, addPrefixed("?", url.query, addPort(url.port_, http + url.host) + url.path));
};
const percentEncode = Dartea_browser.$$Url$percentEncode;
const percentDecode = Dartea_browser.$$Url$percentDecode;
export { Http, Https, Url, fromString, percentDecode, percentEncode, toString };
