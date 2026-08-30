import * as Dartea_runtime from "./Dartea_runtime.mjs";
const toCode = x => Dartea_runtime.$$charToCode(x);
const isUpper = $$char => {
  const code = toCode($$char);
  return (code <= 90) && (65 <= code);
};
const isLower = $$char$1 => {
  const code$1 = toCode($$char$1);
  return (97 <= code$1) && (code$1 <= 122);
};
const isAlpha = $$char$2 => isLower($$char$2) || isUpper($$char$2);
const isDigit = $$char$3 => {
  const code$2 = toCode($$char$3);
  return (code$2 <= 57) && (48 <= code$2);
};
const isAlphaNum = $$char$4 => isLower($$char$4) || (isUpper($$char$4) || isDigit($$char$4));
const isOctDigit = $$char$5 => {
  const code$3 = toCode($$char$5);
  return (code$3 <= 55) && (48 <= code$3);
};
const isHexDigit = $$char$6 => {
  const code$4 = toCode($$char$6);
  return ((48 <= code$4) && (code$4 <= 57)) || (((65 <= code$4) && (code$4 <= 70)) || ((97 <= code$4) && (code$4 <= 102)));
};
const toUpper = x => x.toUpperCase();
const toLower = x => x.toLowerCase();
const fromCode = x => Dartea_runtime.$$charFromCode(x);
export { fromCode, isAlpha, isAlphaNum, isDigit, isHexDigit, isLower, isOctDigit, isUpper, toCode, toLower, toUpper };
