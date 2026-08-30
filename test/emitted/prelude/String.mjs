// Compiled by dartea, an independent compiler. Not affiliated with or
// endorsed by the Elm project.
import * as Dartea_runtime from "./Dartea_runtime.mjs";
import * as Basics from "./Basics.mjs";
import * as Char from "./Char.mjs";
import * as List from "./List.mjs";
import * as Maybe from "./Maybe.mjs";
const length = x => x.length;
const append = (a, b) => a + b;
const split = (a, b) => Dartea_runtime.$$stringSplit(a, b);
const toList = x => Dartea_runtime.$$stringToList(x);
const fromList = x => Dartea_runtime.$$stringFromList(x);
const takeLeft = (a, b) => b.slice(0, a);
const dropLeftBy = (a, b) => b.slice(a);
const isEmpty = text => text === "";
const reverse = text$1 => fromList(List.reverse(toList(text$1)));
const repeatHelp = (n, chunk, result) => {
  while (true) {
    if (n <= 0) {
      return result;
    } else {
      const $s1 = n - 1;
      const $s2 = chunk;
      const $s3 = append(result, chunk);
      n = $s1;
      chunk = $s2;
      result = $s3;
      continue;
    }
  }
};
const repeat = (n$1, chunk$1) => repeatHelp(n$1, chunk$1, "");
const concat = chunks => List.foldr(append, "", chunks);
const join = (sep, chunks$1) => {
  if (chunks$1 === 0) {
    return "";
  } else {
    const first = chunks$1.hd;
    const rest = chunks$1.tl;
    return List.foldl((chunk$2, acc) => append(append(acc, sep), chunk$2), first, rest);
  }
};
const replace = (before, after, text$2) => join(after, split(before, text$2));
const isSpace = $$char => {
  const code = Char.toCode($$char);
  return (code === 32) || ((code === 10) || ((code === 13) || (code === 9)));
};
const wordStep = (letter, chunks$2) => {
  if (isSpace(letter)) {
    return { hd: "", tl: chunks$2 };
  } else {
    if (chunks$2 !== 0) {
      const current = chunks$2.hd;
      const rest$1 = chunks$2.tl;
      return { hd: append(fromList({ hd: letter, tl: 0 }), current), tl: rest$1 };
    } else {
      return { hd: fromList({ hd: letter, tl: 0 }), tl: 0 };
    }
  }
};
const words = text$3 => List.filter(chunk$3 => chunk$3 !== "", List.foldr(wordStep, { hd: "", tl: 0 }, toList(text$3)));
const lines = text$4 => split("\n", join("\n", split("\r\n", text$4)));
const clampIndex = (index, size) => {
  if (index < 0) {
    const $s4 = size + index;
    return (0 > $s4) ? 0 : $s4;
  } else {
    return (index < size) ? index : size;
  }
};
const slice = (start, end, text$5) => {
  const size$1 = length(text$5);
  const from = clampIndex(start, size$1);
  const to = clampIndex(end, size$1);
  if (from >= to) {
    return "";
  } else {
    return takeLeft(to - from, dropLeftBy(from, text$5));
  }
};
const left = (n$2, text$6) => {
  if (n$2 < 1) {
    return "";
  } else {
    return takeLeft(n$2, text$6);
  }
};
const right = (n$3, text$7) => {
  if (n$3 < 1) {
    return "";
  } else {
    const $s5 = length(text$7) - n$3;
    return dropLeftBy((0 > $s5) ? 0 : $s5, text$7);
  }
};
const dropLeft = (n$4, text$8) => {
  if (n$4 < 1) {
    return text$8;
  } else {
    return dropLeftBy(n$4, text$8);
  }
};
const dropRight = (n$5, text$9) => {
  if (n$5 < 1) {
    return text$9;
  } else {
    const $s6 = length(text$9) - n$5;
    return takeLeft((0 > $s6) ? 0 : $s6, text$9);
  }
};
const contains = (needle, text$10) => {
  if (needle === "") {
    return true;
  } else {
    return List.length(split(needle, text$10)) > 1;
  }
};
const startsWith = (prefix, text$11) => left(length(prefix), text$11) === prefix;
const endsWith = (suffix, text$12) => right(length(suffix), text$12) === suffix;
const indexesHelp = (size$2, position, chunks$3, found) => {
  while (true) {
    if (chunks$3 === 0) {
      return List.reverse(found);
    } else {
      const chunk$4 = chunks$3.hd;
      const rest$2 = chunks$3.tl;
      const $s7 = size$2;
      const $s8 = (position + size$2) + length(chunk$4);
      const $s9 = rest$2;
      const $s10 = { hd: position, tl: found };
      size$2 = $s7;
      position = $s8;
      chunks$3 = $s9;
      found = $s10;
      continue;
    }
  }
};
const indexes = (needle$1, text$13) => {
  if (needle$1 === "") {
    return 0;
  } else {
    const $s11 = split(needle$1, text$13);
    if ($s11 === 0) {
      return 0;
    } else {
      const first$1 = $s11.hd;
      const rest$3 = $s11.tl;
      return indexesHelp(length(needle$1), length(first$1), rest$3, 0);
    }
  }
};
const indices = (needle$2, text$14) => indexes(needle$2, text$14);
const isInt = x => (x !== "") && Number.isInteger(Number(x));
const toIntUnsafe = x => Number(x);
const toInt = text$15 => {
  if (isInt(text$15)) {
    return Maybe.Just(toIntUnsafe(text$15));
  } else {
    return Maybe.Nothing;
  }
};
const fromInt = x => String(x);
const isFloat = x => (x !== "") && !isNaN(Number(x));
const toFloatUnsafe = x => Number(x);
const toFloat = text$16 => {
  if (isFloat(text$16)) {
    return Maybe.Just(toFloatUnsafe(text$16));
  } else {
    return Maybe.Nothing;
  }
};
const fromFloat = x => String(x);
const fromChar = $$char$1 => fromList({ hd: $$char$1, tl: 0 });
const cons = ($$char$2, text$17) => append(fromList({ hd: $$char$2, tl: 0 }), text$17);
const uncons = text$18 => {
  const $s12 = toList(text$18);
  if ($s12 === 0) {
    return Maybe.Nothing;
  } else {
    const $$char$3 = $s12.hd;
    const rest$4 = $s12.tl;
    return Maybe.Just([$$char$3, fromList(rest$4)]);
  }
};
const map = (func, text$19) => fromList(List.map(func, toList(text$19)));
const filter = (isGood, text$20) => fromList(List.filter(isGood, toList(text$20)));
const foldl = (func$1, acc$1, text$21) => List.foldl(func$1, acc$1, toList(text$21));
const foldr = (func$2, acc$2, text$22) => List.foldr(func$2, acc$2, toList(text$22));
const any = (isGood$1, text$23) => List.any(isGood$1, toList(text$23));
const all = (isGood$2, text$24) => List.all(isGood$2, toList(text$24));
const toUpper = text$25 => fromList(List.map(Char.toUpper, toList(text$25)));
const toLower = text$26 => fromList(List.map(Char.toLower, toList(text$26)));
const padLeft = (n$6, $$char$4, text$27) => append(repeatHelp(n$6 - length(text$27), fromList({ hd: $$char$4, tl: 0 }), ""), text$27);
const padRight = (n$7, $$char$5, text$28) => append(text$28, repeatHelp(n$7 - length(text$28), fromList({ hd: $$char$5, tl: 0 }), ""));
const pad = (n$8, $$char$6, text$29) => {
  const half = Basics.toFloat(n$8 - length(text$29)) / 2;
  return append(repeatHelp(Basics.ceiling(half), fromList({ hd: $$char$6, tl: 0 }), ""), append(text$29, repeatHelp(Basics.floor(half), fromList({ hd: $$char$6, tl: 0 }), "")));
};
const dropSpaces = chars => {
  while (true) {
    if (chars === 0) {
      return 0;
    } else {
      const $$char$7 = chars.hd;
      const rest$5 = chars.tl;
      if (isSpace($$char$7)) {
        const $s13 = rest$5;
        chars = $s13;
        continue;
      } else {
        return chars;
      }
    }
  }
};
const trimLeft = text$30 => fromList(dropSpaces(toList(text$30)));
const trimRight = text$31 => fromList(List.reverse(dropSpaces(List.reverse(toList(text$31)))));
const trim = text$32 => fromList(dropSpaces(toList(trimRight(text$32))));
export { all, any, append, concat, cons, contains, dropLeft, dropRight, endsWith, filter, foldl, foldr, fromChar, fromFloat, fromInt, fromList, indexes, indices, isEmpty, join, left, length, lines, map, pad, padLeft, padRight, repeat, replace, reverse, right, slice, split, startsWith, toFloat, toInt, toList, toLower, toUpper, trim, trimLeft, trimRight, uncons, words };
