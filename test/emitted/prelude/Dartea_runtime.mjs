// Derived from elm/core -- https://github.com/elm/core
// Copyright 2014-present Evan Czaplicki, BSD 3-Clause License.
// Emitted by dartea; its LICENSE file carries the full text.
const $$curry = (f, args) => {
  const n = f.length === 0 ? 1 : f.length;
  if (args.length === n) return f(...args);
  if (args.length < n) return (...more) => $$curry(f, [...args, ...more]);
  return $$curry(f(...args.slice(0, n)), args.slice(n));
};

const $$eqHelp = (x, y, depth, stack) => {
  if (x === y) return true;
  if (typeof x !== "object" || x === null || y === null) {
    if (typeof x === "function") throw new Error("Functions cannot be compared");
    return false;
  }
  if (depth > 100) {
    stack.push([x, y]);
    return true;
  }
  for (const key in x) {
    if (!$$eqHelp(x[key], y[key], depth + 1, stack)) return false;
  }
  return true;
};

const $$eq = (x, y) => {
  const stack = [];
  let equal = $$eqHelp(x, y, 0, stack);
  for (let pair = stack.pop(); equal && pair !== undefined; pair = stack.pop()) {
    equal = $$eqHelp(pair[0], pair[1], 0, stack);
  }
  return equal;
};

const $$cmp = (x, y) => {
  if (typeof x !== "object" && typeof y !== "object") {
    return x === y ? 0 : x < y ? -1 : 1;
  }
  if (Array.isArray(x)) {
    for (const [index, item] of x.entries()) {
      const ordering = $$cmp(item, y[index]);
      if (ordering !== 0) return ordering;
    }
    return 0;
  }
  let left = x;
  let right = y;
  while (left !== 0 && right !== 0) {
    const ordering = $$cmp(left.hd, right.hd);
    if (ordering !== 0) return ordering;
    left = left.tl;
    right = right.tl;
  }
  return left !== 0 ? 1 : right !== 0 ? -1 : 0;
};

const $$modBy = (modulus, x) => {
  const answer = x % modulus;
  if (modulus === 0) throw new Error("modBy: division by zero");
  return (answer > 0 && modulus < 0) || (answer < 0 && modulus > 0)
    ? answer + modulus
    : answer;
};

const $$charToCode = (char) => char.codePointAt(0);

const $$listOf = (items) => {
  let list = 0;
  for (const item of items.toReversed()) list = { hd: item, tl: list };
  return list;
};

const $$stringToList = (text) => $$listOf([...text]);

const $$stringFromList = (chars) => {
  let text = "";
  for (let rest = chars; rest !== 0; rest = rest.tl) text += rest.hd;
  return text;
};

const $$stringSplit = (separator, text) => $$listOf(text.split(separator));

const $$charFromCode = (code) =>
  code < 0 || 0x10ffff < code ? "\ufffd" : String.fromCodePoint(code);

export {
  $$curry,
  $$eq,
  $$cmp,
  $$modBy,
  $$charToCode,
  $$charFromCode,
  $$stringToList,
  $$stringFromList,
  $$stringSplit,
};
