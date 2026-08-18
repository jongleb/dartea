import * as Maybe from "./Maybe.mjs";
const length = x => x.length;
const append = (a, b) => a + b;
const fromInt = x => String(x);
const fromFloat = x => String(x);
const toFloat = string => {
  if ((string !== "") && !isNaN(Number(string))) {
    return Maybe.Just(Number(string));
  } else {
    return Maybe.Nothing;
  }
};
const toInt = string$1 => {
  if ((string$1 !== "") && Number.isInteger(Number(string$1))) {
    return Maybe.Just(Number(string$1));
  } else {
    return Maybe.Nothing;
  }
};
export { append, fromFloat, fromInt, length, toFloat, toInt };
