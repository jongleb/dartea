const $$Json$isString = (value) => typeof value === "string";
const $$Json$isBool = (value) => typeof value === "boolean";
const $$Json$isNumber = (value) => typeof value === "number";
const $$Json$isInt = (value) => Number.isInteger(value);
const $$Json$isNull = (value) => value === null;
const $$Json$isArray = (value) => Array.isArray(value);
const $$Json$isObject = (value) =>
  typeof value === "object" && value !== null && !Array.isArray(value);
const $$Json$hasField = (name, value) =>
  Object.prototype.hasOwnProperty.call(value, name);
const $$Json$unsafeField = (name, value) => value[name];
const $$Json$length = (value) => value.length;
const $$Json$unsafeIndex = (index, value) => value[index];
const $$Json$identity = (value) => value;
const $$Json$stringify = (value) => JSON.stringify(value) ?? "null";
const $$Json$isValid = (text) => {
  try {
    JSON.parse(text);
    return true;
  } catch {
    return false;
  }
};
const $$Json$unsafeParse = (text) => JSON.parse(text);
const $$Json$emptyArray = [];
const $$Json$pushed = (item, array) => [...array, item];
const $$Json$emptyObject = {};
const $$Json$withField = (name, value, object) => ({ ...object, [name]: value });

export {
  $$Json$isString,
  $$Json$isBool,
  $$Json$isNumber,
  $$Json$isInt,
  $$Json$isNull,
  $$Json$isArray,
  $$Json$isObject,
  $$Json$hasField,
  $$Json$unsafeField,
  $$Json$length,
  $$Json$unsafeIndex,
  $$Json$identity,
  $$Json$stringify,
  $$Json$isValid,
  $$Json$unsafeParse,
  $$Json$emptyArray,
  $$Json$pushed,
  $$Json$emptyObject,
  $$Json$withField,
};
