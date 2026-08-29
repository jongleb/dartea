// Compiled by dartea, an independent compiler. Not affiliated with or
// endorsed by the Elm project.
// Contains material derived from elm/core,
// Copyright 2014-present Evan Czaplicki, under the BSD 3-Clause License.
// dartea's LICENSE carries the full text.
import * as Dartea_runtime from "./Dartea_runtime.mjs";
const Just = _0 => ({ _0: _0 });
const Nothing = "Nothing";
const withDefault = ($$default, maybe) => {
  if (typeof maybe === "object") {
    const value = maybe._0;
    return value;
  } else {
    return $$default;
  }
};
const map = (f, maybe$1) => {
  if (typeof maybe$1 === "object") {
    const value$1 = maybe$1._0;
    return Just(Dartea_runtime.$$curry(f, [value$1]));
  } else {
    return Nothing;
  }
};
const andThen = (callback, maybeValue) => {
  if (typeof maybeValue === "object") {
    const value$2 = maybeValue._0;
    return callback(value$2);
  } else {
    return Nothing;
  }
};
const map2 = (func, ma, mb) => {
  if (ma === "Nothing") {
    return Nothing;
  } else {
    const a = ma._0;
    if (mb === "Nothing") {
      return Nothing;
    } else {
      const b = mb._0;
      return Just(Dartea_runtime.$$curry(func, [a, b]));
    }
  }
};
const map3 = (func$1, ma$1, mb$1, mc) => {
  if (ma$1 === "Nothing") {
    return Nothing;
  } else {
    const a$1 = ma$1._0;
    if (mb$1 === "Nothing") {
      return Nothing;
    } else {
      const b$1 = mb$1._0;
      if (mc === "Nothing") {
        return Nothing;
      } else {
        const c = mc._0;
        return Just(Dartea_runtime.$$curry(func$1, [a$1, b$1, c]));
      }
    }
  }
};
const map4 = (func$2, ma$2, mb$2, mc$1, md) => {
  if (ma$2 === "Nothing") {
    return Nothing;
  } else {
    const a$2 = ma$2._0;
    if (mb$2 === "Nothing") {
      return Nothing;
    } else {
      const b$2 = mb$2._0;
      if (mc$1 === "Nothing") {
        return Nothing;
      } else {
        const c$1 = mc$1._0;
        if (md === "Nothing") {
          return Nothing;
        } else {
          const d = md._0;
          return Just(Dartea_runtime.$$curry(func$2, [a$2, b$2, c$1, d]));
        }
      }
    }
  }
};
const map5 = (func$3, ma$3, mb$3, mc$2, md$1, me) => {
  if (ma$3 === "Nothing") {
    return Nothing;
  } else {
    const a$3 = ma$3._0;
    if (mb$3 === "Nothing") {
      return Nothing;
    } else {
      const b$3 = mb$3._0;
      if (mc$2 === "Nothing") {
        return Nothing;
      } else {
        const c$2 = mc$2._0;
        if (md$1 === "Nothing") {
          return Nothing;
        } else {
          const d$1 = md$1._0;
          if (me === "Nothing") {
            return Nothing;
          } else {
            const e = me._0;
            return Just(Dartea_runtime.$$curry(func$3, [a$3, b$3, c$2, d$1, e]));
          }
        }
      }
    }
  }
};
export { Just, Nothing, andThen, map, map2, map3, map4, map5, withDefault };
