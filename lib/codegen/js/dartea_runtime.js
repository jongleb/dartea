// Derived from elm/core -- https://github.com/elm/core
// Copyright 2014-present Evan Czaplicki, BSD 3-Clause License.
// Emitted by dartea; its LICENSE file carries the full text.
const $$curry = (f, args) => {
  const n = f.length === 0 ? 1 : f.length;
  if (args.length === n) return f(...args);
  if (args.length < n) return (...more) => $$curry(f, [...args, ...more]);
  return $$curry(f(...args.slice(0, n)), args.slice(n));
};

const $$append = (xs, ys) => {
  if (typeof xs === "string") return xs + ys;
  if (xs === 0) return ys;
  const root = { hd: xs.hd, tl: ys };
  let last = root;
  for (let rest = xs.tl; rest !== 0; rest = rest.tl) {
    last = last.tl = { hd: rest.hd, tl: ys };
  }
  return root;
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
    for (let index = 0; index < x.length; index++) {
      const ordering = $$cmp(x[index], y[index]);
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

const $$charToCode = (char) => {
  const code = char.charCodeAt(0);
  if (0xd800 <= code && code <= 0xdbff) {
    return (code - 0xd800) * 0x400 + char.charCodeAt(1) - 0xdc00 + 0x10000;
  }
  return code;
};

const $$charFromCode = (code) => {
  if (code < 0 || 0x10ffff < code) return "\ufffd";
  if (code <= 0xffff) return String.fromCharCode(code);
  const rest = code - 0x10000;
  return String.fromCharCode(
    Math.floor(rest / 0x400) + 0xd800,
    (rest % 0x400) + 0xdc00,
  );
};

export { $$curry, $$append, $$eq, $$cmp, $$modBy, $$charToCode, $$charFromCode };
