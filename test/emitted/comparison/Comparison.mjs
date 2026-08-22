import * as Basics from "./Basics.mjs";
import * as Char from "./Char.mjs";
import * as Maybe from "./Maybe.mjs";
const Leaf = "Leaf";
const Node = (_0, _1, _2) => ({ _0: _0, _1: _1, _2: _2 });
const Dot = "Dot";
const Line = (_0, _1) => ({ TAG: "Line", _0: _0, _1: _1 });
const Named = _0 => ({ TAG: "Named", _0: _0 });
const Red = "Red";
const Green = "Green";
const Blue = "Blue";
const Boxed = _0 => ({ _0: _0 });
const $eq$List$number = (xs, ys) => {
  let left = xs;
  let right = ys;
  while ((left !== 0) && (right !== 0)) {
    if (!(left.hd === right.hd)) {
      return false;
    }
    left = left.tl;
    right = right.tl;
  }
  return left === right;
};
const $cmp$List$number = (xs, ys) => {
  let left = xs;
  let right = ys;
  while ((left !== 0) && (right !== 0)) {
    const ordering = (left.hd === right.hd) ? 0 : (left.hd < right.hd) ? -1 : 1;
    if (ordering !== 0) {
      return ordering;
    }
    left = left.tl;
    right = right.tl;
  }
  return (left !== 0) ? 1 : (right !== 0) ? -1 : 0;
};
const $eq$List$String = (xs, ys) => {
  let left = xs;
  let right = ys;
  while ((left !== 0) && (right !== 0)) {
    if (!(left.hd === right.hd)) {
      return false;
    }
    left = left.tl;
    right = right.tl;
  }
  return left === right;
};
const $eq$List$Record$x$number$y$String = (xs, ys) => {
  let left = xs;
  let right = ys;
  while ((left !== 0) && (right !== 0)) {
    if (!((left.hd.x === right.hd.x) && (left.hd.y === right.hd.y))) {
      return false;
    }
    left = left.tl;
    right = right.tl;
  }
  return left === right;
};
const $eq$Shape = (a, b) => {
  if (!(typeof a === "object")) {
    return a === b;
  }
  if (!(typeof b === "object")) {
    return false;
  }
  if (a.TAG !== b.TAG) {
    return false;
  }
  switch (a.TAG) {
    case "Line":
      return (a._0 === b._0) && (a._1 === b._1);
    default:
      return a._0 === b._0;
  }
};
const $eq$Boxed = (a, b) => {
  if (!(typeof a === "object")) {
    return a === b;
  }
  if (!(typeof b === "object")) {
    return false;
  }
  return a._0 === b._0;
};
const $eq$Tree = (a, b) => {
  if (!(typeof a === "object")) {
    return a === b;
  }
  if (!(typeof b === "object")) {
    return false;
  }
  return ($eq$Tree(a._0, b._0) && (a._1 === b._1)) && $eq$Tree(a._2, b._2);
};
const $eq$Maybe$Maybe$number = (a, b) => {
  if (!(typeof a === "object")) {
    return a === b;
  }
  if (!(typeof b === "object")) {
    return false;
  }
  return a._0 === b._0;
};
const $eq$List$Shape = (xs, ys) => {
  let left = xs;
  let right = ys;
  while ((left !== 0) && (right !== 0)) {
    if (!$eq$Shape(left.hd, right.hd)) {
      return false;
    }
    left = left.tl;
    right = right.tl;
  }
  return left === right;
};
const $cmp$Tuple$number$String = (a, b) => {
  const ordering0 = (a[0] === b[0]) ? 0 : (a[0] < b[0]) ? -1 : 1;
  if (ordering0 !== 0) {
    return ordering0;
  }
  const ordering1 = (a[1] === b[1]) ? 0 : (a[1] < b[1]) ? -1 : 1;
  if (ordering1 !== 0) {
    return ordering1;
  }
  return 0;
};
const $append$List = (xs, ys) => {
  if (xs === 0) {
    return ys;
  }
  const root = { hd: xs.hd, tl: ys };
  let last = root;
  let rest = xs.tl;
  while (rest !== 0) {
    const copied = { hd: rest.hd, tl: ys };
    last.tl = copied;
    last = copied;
    rest = rest.tl;
  }
  return root;
};
const $eq$List$Int = (xs, ys) => {
  let left = xs;
  let right = ys;
  while ((left !== 0) && (right !== 0)) {
    if (!(left.hd === right.hd)) {
      return false;
    }
    left = left.tl;
    right = right.tl;
  }
  return left === right;
};
const floats = 1.5 < 2.5;
const chars = "a" < "b";
const strings = "apple" < "pear";
const booleans = true === true;
const units = null === null;
const enums = Red === Red;
const enumsDiffer = Red === Green;
const $s1 = { x: 1, y: "a" };
const $s2 = { x: 1, y: "a" };
const records = ($s1.x === $s2.x) && ($s1.y === $s2.y);
const $s3 = { x: 1, y: "a" };
const $s4 = { x: 2, y: "a" };
const recordsDiffer = ($s3.x === $s4.x) && ($s3.y === $s4.y);
const $s5 = [1, "a"];
const $s6 = [1, "a"];
const tuples = ($s5[0] === $s6[0]) && ($s5[1] === $s6[1]);
const $s7 = [1, "a"];
const $s8 = [1, "b"];
const tuplesOrdered = ($s7[0] === $s8[0]) ? $s7[1] < $s8[1] : $s7[0] < $s8[0];
const $s9 = [1, "z"];
const $s10 = [2, "a"];
const tuplesLexicographic = ($s9[0] === $s10[0]) ? $s9[1] < $s10[1] : $s9[0] < $s10[0];
const $s11 = [1, ["a", "c"]];
const $s12 = [1, ["a", "c"]];
const nestedTuples = ($s11[0] === $s12[0]) && (($s11[1][0] === $s12[1][0]) && ($s11[1][1] === $s12[1][1]));
const listsOfNumbers = $eq$List$number({ hd: 1, tl: { hd: 2, tl: { hd: 3, tl: 0 } } }, { hd: 1, tl: { hd: 2, tl: { hd: 3, tl: 0 } } });
const listsOfNumbersOrdered = $cmp$List$number({ hd: 1, tl: { hd: 2, tl: 0 } }, { hd: 1, tl: { hd: 3, tl: 0 } }) < 0;
const listsOfNumbersPrefix = $cmp$List$number({ hd: 1, tl: 0 }, { hd: 1, tl: { hd: 2, tl: 0 } }) < 0;
const emptyIsSmallest = $cmp$List$number(0, { hd: 1, tl: 0 }) < 0;
const listsOfStrings = $eq$List$String({ hd: "x", tl: { hd: "y", tl: 0 } }, { hd: "x", tl: { hd: "y", tl: 0 } });
const listsOfRecords = $eq$List$Record$x$number$y$String({ hd: { x: 1, y: "a" }, tl: 0 }, { hd: { x: 1, y: "a" }, tl: 0 });
const variants = $eq$Shape(Line(1, 2), Line(1, 2));
const variantsDiffer = $eq$Shape(Line(1, 2), Line(1, 3));
const variantsAcrossTags = $eq$Shape(Named("a"), Line(1, 2));
const variantsNullary = $eq$Shape(Dot, Dot);
const taggedOmitted = $eq$Boxed(Boxed(1.5), Boxed(1.5));
const recursive = $eq$Tree(Node(Node(Leaf, 1, Leaf), 2, Leaf), Node(Node(Leaf, 1, Leaf), 2, Leaf));
const recursiveDiffer = $eq$Tree(Node(Node(Leaf, 1, Leaf), 2, Leaf), Node(Node(Leaf, 9, Leaf), 2, Leaf));
const maybes = $eq$Maybe$Maybe$number(Maybe.Just(1), Maybe.Just(1));
const maybesAgainstNothing = $eq$Maybe$Maybe$number(Maybe.Just(1), Maybe.Nothing);
const listsOfVariants = $eq$List$Shape({ hd: Dot, tl: { hd: Line(1, 2), tl: 0 } }, { hd: Dot, tl: { hd: Line(1, 2), tl: 0 } });
const smallestNumber = (4 < 2) ? 4 : 2;
const largestWord = ("apple" > "pear") ? "apple" : "pear";
const $s13 = (1 === 2) ? 0 : (1 < 2) ? -1 : 1;
const comparedNumbers = ($s13 < 0) ? "LT" : ($s13 === 0) ? "EQ" : "GT";
const $s14 = [1, "b"];
const $s15 = [1, "a"];
const $s16 = $cmp$Tuple$number$String($s14, $s15);
const comparedTuples = ($s16 < 0) ? "LT" : ($s16 === 0) ? "EQ" : "GT";
const $s17 = { hd: 1, tl: { hd: 2, tl: 0 } };
const $s18 = { hd: 1, tl: { hd: 3, tl: 0 } };
const $s19 = $cmp$List$number($s17, $s18);
const comparedLists = ($s19 < 0) ? "LT" : ($s19 === 0) ? "EQ" : "GT";
const $s20 = [2, "a"];
const $s21 = [1, "z"];
const smallestTuple = (($s20[0] === $s21[0]) ? $s20[1] < $s21[1] : $s20[0] < $s21[0]) ? $s20 : $s21;
const appendedLists = $append$List({ hd: 1, tl: { hd: 2, tl: 0 } }, { hd: 3, tl: 0 });
const grinning = Char.toCode("😀");
const tree = Char.toCode("木");
const $s22 = [1, "z"];
const one = { hd: 1, tl: 0 };
const other = { hd: 2, tl: 0 };
const one$1 = { hd: 1, tl: { hd: 2, tl: 0 } };
const other$1 = { hd: 1, tl: { hd: 2, tl: 0 } };
const one$2 = { x: 1, y: "a" };
const other$2 = { x: 2, y: "a" };
const checks = { hd: true, tl: { hd: floats, tl: { hd: chars, tl: { hd: strings, tl: { hd: booleans, tl: { hd: units, tl: { hd: enums, tl: { hd: Basics.not(enumsDiffer), tl: { hd: records, tl: { hd: Basics.not(recordsDiffer), tl: { hd: tuples, tl: { hd: tuplesOrdered, tl: { hd: tuplesLexicographic, tl: { hd: nestedTuples, tl: { hd: listsOfNumbers, tl: { hd: listsOfNumbersOrdered, tl: { hd: listsOfNumbersPrefix, tl: { hd: emptyIsSmallest, tl: { hd: listsOfStrings, tl: { hd: listsOfRecords, tl: { hd: variants, tl: { hd: Basics.not(variantsDiffer), tl: { hd: Basics.not(variantsAcrossTags), tl: { hd: variantsNullary, tl: { hd: taggedOmitted, tl: { hd: recursive, tl: { hd: Basics.not(recursiveDiffer), tl: { hd: maybes, tl: { hd: Basics.not(maybesAgainstNothing), tl: { hd: listsOfVariants, tl: { hd: smallestNumber === 2, tl: { hd: largestWord === "pear", tl: { hd: comparedNumbers === Basics.LT, tl: { hd: comparedTuples === Basics.GT, tl: { hd: comparedLists === Basics.LT, tl: { hd: (smallestTuple[0] === $s22[0]) && (smallestTuple[1] === $s22[1]), tl: { hd: true, tl: { hd: $eq$List$Int(appendedLists, { hd: 1, tl: { hd: 2, tl: { hd: 3, tl: 0 } } }), tl: { hd: true, tl: { hd: $eq$List$number($append$List(one, other), { hd: 1, tl: { hd: 2, tl: 0 } }), tl: { hd: $eq$List$number(one$1, other$1), tl: { hd: Basics.not((one$2.x === other$2.x) && (one$2.y === other$2.y)), tl: { hd: true, tl: { hd: (("b" < "a") ? "b" : "a") === "a", tl: { hd: grinning === 128512, tl: { hd: tree === 26408, tl: 0 } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } };
const allTrue = flags => {
  if (flags === 0) {
    return true;
  } else {
    const flag = flags.hd;
    const rest = flags.tl;
    return flag && allTrue(rest);
  }
};
const report = allTrue(checks) ? "all instances agree" : "SOMETHING DISAGREES";
export { checks, report };
