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
export { Just, Nothing, andThen, map, withDefault };
