// Compiled by dartea, an independent compiler. Not affiliated with or
// endorsed by the Elm project.
// Contains material derived from elm/core,
// Copyright 2014-present Evan Czaplicki, under the BSD 3-Clause License.
// dartea's LICENSE carries the full text.
import * as Dartea_runtime from "./Dartea_runtime.mjs";
import * as Maybe from "./Maybe.mjs";
const Ok = _0 => ({ TAG: "Ok", _0: _0 });
const Err = _0 => ({ TAG: "Err", _0: _0 });
const withDefault = (def, result) => {
  switch (result.TAG) {
    case "Ok":
      const a = result._0;
      return a;
    case "Err":
      return def;
  }
};
const map = (func, ra) => {
  switch (ra.TAG) {
    case "Ok":
      const a$1 = ra._0;
      return Ok(Dartea_runtime.$$curry(func, [a$1]));
    case "Err":
      const e = ra._0;
      return Err(e);
  }
};
const map2 = (func$1, ra$1, rb) => {
  switch (ra$1.TAG) {
    case "Err":
      const x = ra$1._0;
      return Err(x);
    case "Ok":
      const a$2 = ra$1._0;
      switch (rb.TAG) {
        case "Err":
          const x$1 = rb._0;
          return Err(x$1);
        case "Ok":
          const b = rb._0;
          return Ok(Dartea_runtime.$$curry(func$1, [a$2, b]));
      }
  }
};
const map3 = (func$2, ra$2, rb$1, rc) => {
  switch (ra$2.TAG) {
    case "Err":
      const x$2 = ra$2._0;
      return Err(x$2);
    case "Ok":
      const a$3 = ra$2._0;
      switch (rb$1.TAG) {
        case "Err":
          const x$3 = rb$1._0;
          return Err(x$3);
        case "Ok":
          const b$1 = rb$1._0;
          switch (rc.TAG) {
            case "Err":
              const x$4 = rc._0;
              return Err(x$4);
            case "Ok":
              const c = rc._0;
              return Ok(Dartea_runtime.$$curry(func$2, [a$3, b$1, c]));
          }
      }
  }
};
const map4 = (func$3, ra$3, rb$2, rc$1, rd) => map2((g, d) => Dartea_runtime.$$curry(g, [d]), map3(func$3, ra$3, rb$2, rc$1), rd);
const map5 = (func$4, ra$4, rb$3, rc$2, rd$1, re) => map2((g$1, e$1) => Dartea_runtime.$$curry(g$1, [e$1]), map4(func$4, ra$4, rb$3, rc$2, rd$1), re);
const andThen = (callback, result$1) => {
  switch (result$1.TAG) {
    case "Ok":
      const value = result$1._0;
      return callback(value);
    case "Err":
      const msg = result$1._0;
      return Err(msg);
  }
};
const mapError = (f, result$2) => {
  switch (result$2.TAG) {
    case "Ok":
      const v = result$2._0;
      return Ok(v);
    case "Err":
      const e$2 = result$2._0;
      return Err(Dartea_runtime.$$curry(f, [e$2]));
  }
};
const toMaybe = result$3 => {
  switch (result$3.TAG) {
    case "Ok":
      const v$1 = result$3._0;
      return Maybe.Just(v$1);
    case "Err":
      return Maybe.Nothing;
  }
};
const fromMaybe = (err, maybe) => {
  if (typeof maybe === "object") {
    const v$2 = maybe._0;
    return Ok(v$2);
  } else {
    return Err(err);
  }
};
export { Err, Ok, andThen, fromMaybe, map, map2, map3, map4, map5, mapError, toMaybe, withDefault };
