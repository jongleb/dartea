const $$curry = (f, args) => {
  const n = f.length === 0 ? 1 : f.length;
  if (args.length === n) return f(...args);
  if (args.length < n) return (...more) => $$curry(f, [...args, ...more]);
  return $$curry(f(...args.slice(0, n)), args.slice(n));
};

const $$apply1 = (f, x) => (f.length === 1 ? f(x) : $$curry(f, [x]));
const $$apply2 = (f, x, y) => (f.length === 2 ? f(x, y) : $$curry(f, [x, y]));

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

const $$eq = (x, y) => {
  const pending = [[x, y]];
  while (pending.length > 0) {
    const [left, right] = pending.pop();
    if (left === right) continue;
    if (typeof left === "function") throw new Error("Functions cannot be compared");
    if (typeof left !== "object" || left === null || right === null) return false;
    for (const key of Object.keys(left)) pending.push([left[key], right[key]]);
  }
  return true;
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
  $$apply1,
  $$apply2,
  $$append,
  $$eq,
  $$cmp,
  $$modBy,
  $$charToCode,
  $$charFromCode,
  $$stringToList,
  $$stringFromList,
  $$stringSplit,
};
