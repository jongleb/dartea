import * as Dartea_runtime from "./Dartea_runtime.mjs";
import * as Dartea_parser from "./Dartea_parser.mjs";
import * as Basics from "./Basics.mjs";
import * as Char from "./Char.mjs";
import * as List from "./List.mjs";
import * as Maybe from "./Maybe.mjs";
import * as Result from "./Result.mjs";
import * as $$Set from "./Set.mjs";
import * as $$String from "./String.mjs";
import * as Tuple from "./Tuple.mjs";
const Forbidden = "Forbidden";
const Optional = "Optional";
const Mandatory = "Mandatory";
const Token = (_0, _1) => ({ _0: _0, _1: _1 });
const Loop = _0 => ({ TAG: "Loop", _0: _0 });
const Done = _0 => ({ TAG: "Done", _0: _0 });
const Parser = _0 => ({ _0: _0 });
const Good = (_0, _1, _2) => ({ TAG: "Good", _0: _0, _1: _1, _2: _2 });
const Bad = (_0, _1) => ({ TAG: "Bad", _0: _0, _1: _1 });
const NotNestable = "NotNestable";
const Nestable = "Nestable";
const Empty = "Empty";
const AddRight = (_0, _1) => ({ TAG: "AddRight", _0: _0, _1: _1 });
const Append = (_0, _1) => ({ TAG: "Append", _0: _0, _1: _1 });
const map2 = (func, $p1, $p2) => {
  const parseA = $p1._0;
  const parseB = $p2._0;
  return Parser(s0 => {
  const $s1 = parseA(s0);
  switch ($s1.TAG) {
    case "Bad":
      const p = $s1._0;
      const x = $s1._1;
      return Bad(p, x);
    case "Good":
      const p1 = $s1._0;
      const a = $s1._1;
      const s1 = $s1._2;
      const $s2 = parseB(s1);
      switch ($s2.TAG) {
        case "Bad":
          const p2 = $s2._0;
          const x$1 = $s2._1;
          return Bad(p1 || p2, x$1);
        case "Good":
          const p2$1 = $s2._0;
          const b = $s2._1;
          const s2 = $s2._2;
          return Good(p1 || p2$1, Dartea_runtime.$$apply2(func, a, b), s2);
      }
  }
});
};
const $$pipe$eq = (eta1, eta2) => map2(Basics.apL, eta1, eta2);
const $$pipe$dot = (eta1$1, eta2$1) => map2(Basics.always, eta1$1, eta2$1);
const bagToList = (bag, list) => {
  while (true) {
    if (bag === "Empty") {
      return list;
    } else {
      if (bag.TAG === "AddRight") {
        const bag1$1 = bag._0;
        const x$2 = bag._1;
        const $s5 = bag1$1;
        const $s6 = { hd: x$2, tl: list };
        bag = $s5;
        list = $s6;
        continue;
      } else {
        const bag1 = bag._0;
        const bag2 = bag._1;
        const $s3 = bag1;
        const $s4 = bagToList(bag2, list);
        bag = $s3;
        list = $s4;
        continue;
      }
    }
  }
};
const run = ($p0, src) => {
  const parse = $p0._0;
  const $s7 = parse({ src: src, offset: 0, indent: 1, context: 0, row: 1, col: 1 });
  switch ($s7.TAG) {
    case "Good":
      const value = $s7._1;
      return Result.Ok(value);
    case "Bad":
      const bag$1 = $s7._1;
      return Result.Err(bagToList(bag$1, 0));
  }
};
const DeadEnd = ($a0, $a1, $a2, $a3) => ({ row: $a0, col: $a1, problem: $a2, contextStack: $a3 });
const fromState = (s, x$3) => AddRight(Empty, DeadEnd(s.row, s.col, x$3, s.context));
const fromInfo = (row, col, x$4, context) => AddRight(Empty, DeadEnd(row, col, x$4, context));
const succeed = a$1 => Parser(s$1 => Good(false, a$1, s$1));
const problem = x$5 => Parser(s$2 => Bad(false, fromState(s$2, x$5)));
const map = (func$1, $p1$1) => {
  const parse$1 = $p1$1._0;
  return Parser(s0$1 => {
  const $s8 = parse$1(s0$1);
  switch ($s8.TAG) {
    case "Good":
      const p$1 = $s8._0;
      const a$2 = $s8._1;
      const s1$1 = $s8._2;
      return Good(p$1, Dartea_runtime.$$apply1(func$1, a$2), s1$1);
    case "Bad":
      const p$2 = $s8._0;
      const x$6 = $s8._1;
      return Bad(p$2, x$6);
  }
});
};
const andThen = (callback, $p1$2) => {
  const parseA$1 = $p1$2._0;
  return Parser(s0$2 => {
  const $s9 = parseA$1(s0$2);
  switch ($s9.TAG) {
    case "Bad":
      const p$3 = $s9._0;
      const x$7 = $s9._1;
      return Bad(p$3, x$7);
    case "Good":
      const p1$1 = $s9._0;
      const a$3 = $s9._1;
      const s1$2 = $s9._2;
      const $s10 = callback(a$3);
      const parseB$1 = $s10._0;
      const $s11 = parseB$1(s1$2);
      switch ($s11.TAG) {
        case "Bad":
          const p2$2 = $s11._0;
          const x$8 = $s11._1;
          return Bad(p1$1 || p2$2, x$8);
        case "Good":
          const p2$3 = $s11._0;
          const b$1 = $s11._1;
          const s2$1 = $s11._2;
          return Good(p1$1 || p2$3, b$1, s2$1);
      }
  }
});
};
const lazy = thunk => Parser(s$3 => {
  const $s12 = thunk(null);
  const parse$2 = $s12._0;
  return parse$2(s$3);
});
const oneOfHelp = (s0$3, bag$2, parsers) => {
  while (true) {
    if (parsers === 0) {
      return Bad(false, bag$2);
    } else {
      const parse$3 = parsers.hd._0;
      const remainingParsers = parsers.tl;
      const $s13 = parse$3(s0$3);
      switch ($s13.TAG) {
        case "Good":
          const step = $s13;
          return step;
        case "Bad":
          const step$1 = $s13;
          const p$4 = $s13._0;
          const x$9 = $s13._1;
          if (p$4) {
            return step$1;
          } else {
            const $s14 = s0$3;
            const $s15 = Append(bag$2, x$9);
            const $s16 = remainingParsers;
            s0$3 = $s14;
            bag$2 = $s15;
            parsers = $s16;
            continue;
          }
      }
    }
  }
};
const oneOf = parsers$1 => Parser(s$4 => oneOfHelp(s$4, Empty, parsers$1));
const loopHelp = (p$5, state, callback$1, s0$4) => {
  while (true) {
    const $s17 = callback$1(state);
    const parse$4 = $s17._0;
    const $s18 = parse$4(s0$4);
    switch ($s18.TAG) {
      case "Good":
        const p1$2 = $s18._0;
        const step$2 = $s18._1;
        const s1$3 = $s18._2;
        switch (step$2.TAG) {
          case "Loop":
            const newState = step$2._0;
            const $s19 = p$5 || p1$2;
            const $s20 = newState;
            const $s21 = callback$1;
            const $s22 = s1$3;
            p$5 = $s19;
            state = $s20;
            callback$1 = $s21;
            s0$4 = $s22;
            continue;
          case "Done":
            const result = step$2._0;
            return Good(p$5 || p1$2, result, s1$3);
        }
      case "Bad":
        const p1$3 = $s18._0;
        const x$10 = $s18._1;
        return Bad(p$5 || p1$3, x$10);
    }
  }
};
const loop = (state$1, callback$2) => Parser(s$5 => loopHelp(false, state$1, callback$2, s$5));
const backtrackable = $p0$1 => {
  const parse$5 = $p0$1._0;
  return Parser(s0$5 => {
  const $s23 = parse$5(s0$5);
  switch ($s23.TAG) {
    case "Bad":
      const x$11 = $s23._1;
      return Bad(false, x$11);
    case "Good":
      const a$4 = $s23._1;
      const s1$4 = $s23._2;
      return Good(false, a$4, s1$4);
  }
});
};
const commit = a$5 => Parser(s$6 => Good(true, a$5, s$6));
const token = $p0$2 => {
  const str = $p0$2._0;
  const expecting = $p0$2._1;
  const progress = Basics.not($$String.isEmpty(str));
  return Parser(s$7 => {
  const $s24 = Dartea_parser.$$Parser$isSubString(str, s$7.offset, s$7.row, s$7.col, s$7.src);
  const newOffset = $s24[0];
  const newRow = $s24[1];
  const newCol = $s24[2];
  if (newOffset === Basics.negate(1)) {
    return Bad(false, fromState(s$7, expecting));
  } else {
    return Good(progress, null, { src: s$7.src, offset: newOffset, indent: s$7.indent, context: s$7.context, row: newRow, col: newCol });
  }
});
};
const symbol = eta1$2 => token(eta1$2);
const keyword = $p0$3 => {
  const kwd = $p0$3._0;
  const expecting$1 = $p0$3._1;
  const progress$1 = Basics.not($$String.isEmpty(kwd));
  return Parser(s$8 => {
  const $s25 = Dartea_parser.$$Parser$isSubString(kwd, s$8.offset, s$8.row, s$8.col, s$8.src);
  const newOffset$1 = $s25[0];
  const newRow$1 = $s25[1];
  const newCol$1 = $s25[2];
  if ((newOffset$1 === Basics.negate(1)) || (0 <= Dartea_parser.$$Parser$isSubChar(c => Char.isAlphaNum(c) || (c === "_"), newOffset$1, s$8.src))) {
    return Bad(false, fromState(s$8, expecting$1));
  } else {
    return Good(progress$1, null, { src: s$8.src, offset: newOffset$1, indent: s$8.indent, context: s$8.context, row: newRow$1, col: newCol$1 });
  }
});
};
const bumpOffset = (newOffset$2, s$9) => ({ src: s$9.src, offset: newOffset$2, indent: s$9.indent, context: s$9.context, row: s$9.row, col: s$9.col + (newOffset$2 - s$9.offset) });
const consumeExp = (offset, src$1) => {
  if (Dartea_parser.$$Parser$isAsciiCode(101, offset, src$1) || Dartea_parser.$$Parser$isAsciiCode(69, offset, src$1)) {
    const eOffset = offset + 1;
    const expOffset = (Dartea_parser.$$Parser$isAsciiCode(43, eOffset, src$1) || Dartea_parser.$$Parser$isAsciiCode(45, eOffset, src$1)) ? eOffset + 1 : eOffset;
    const newOffset$3 = Dartea_parser.$$Parser$chompBase10(expOffset, src$1);
    if (expOffset === newOffset$3) {
      return Basics.negate(newOffset$3);
    } else {
      return newOffset$3;
    }
  } else {
    return offset;
  }
};
const consumeDotAndExp = (offset$1, src$2) => {
  if (Dartea_parser.$$Parser$isAsciiCode(46, offset$1, src$2)) {
    return consumeExp(Dartea_parser.$$Parser$chompBase10(offset$1 + 1, src$2), src$2);
  } else {
    return consumeExp(offset$1, src$2);
  }
};
const finalizeInt = (invalid, handler, startOffset, $p3, s$10) => {
  const endOffset = $p3[0];
  const n = $p3[1];
  switch (handler.TAG) {
    case "Err":
      const x$12 = handler._0;
      return Bad(true, fromState(s$10, x$12));
    case "Ok":
      const toValue = handler._0;
      if (startOffset === endOffset) {
        return Bad(s$10.offset < startOffset, fromState(s$10, invalid));
      } else {
        return Good(true, Dartea_runtime.$$apply1(toValue, n), bumpOffset(endOffset, s$10));
      }
  }
};
const finalizeFloat = (invalid$1, expecting$2, intSettings, floatSettings, intPair, s$11) => {
  const intOffset = Tuple.first(intPair);
  const floatOffset = consumeDotAndExp(intOffset, s$11.src);
  if (floatOffset < 0) {
    return Bad(true, fromInfo(s$11.row, s$11.col - (floatOffset + s$11.offset), invalid$1, s$11.context));
  } else {
    if (s$11.offset === floatOffset) {
      return Bad(false, fromState(s$11, expecting$2));
    } else {
      if (intOffset === floatOffset) {
        return finalizeInt(invalid$1, intSettings, s$11.offset, intPair, s$11);
      } else {
        switch (floatSettings.TAG) {
          case "Err":
            const x$13 = floatSettings._0;
            return Bad(true, fromState(s$11, invalid$1));
          case "Ok":
            const toValue$1 = floatSettings._0;
            const $s26 = $$String.toFloat($$String.slice(s$11.offset, floatOffset, s$11.src));
            if ($s26 === "Nothing") {
              return Bad(true, fromState(s$11, invalid$1));
            } else {
              const n$1 = $s26._0;
              return Good(true, Dartea_runtime.$$apply1(toValue$1, n$1), bumpOffset(floatOffset, s$11));
            }
        }
      }
    }
  }
};
const number = c$1 => Parser(s$12 => {
  if (Dartea_parser.$$Parser$isAsciiCode(48, s$12.offset, s$12.src)) {
    const zeroOffset = s$12.offset + 1;
    const baseOffset = zeroOffset + 1;
    if (Dartea_parser.$$Parser$isAsciiCode(120, zeroOffset, s$12.src)) {
      return finalizeInt(c$1.invalid, c$1.hex, baseOffset, Dartea_parser.$$Parser$consumeBase16(baseOffset, s$12.src), s$12);
    } else {
      if (Dartea_parser.$$Parser$isAsciiCode(111, zeroOffset, s$12.src)) {
        return finalizeInt(c$1.invalid, c$1.octal, baseOffset, Dartea_parser.$$Parser$consumeBase(8, baseOffset, s$12.src), s$12);
      } else {
        if (Dartea_parser.$$Parser$isAsciiCode(98, zeroOffset, s$12.src)) {
          return finalizeInt(c$1.invalid, c$1.binary, baseOffset, Dartea_parser.$$Parser$consumeBase(2, baseOffset, s$12.src), s$12);
        } else {
          return finalizeFloat(c$1.invalid, c$1.expecting, c$1.int, c$1.float, [zeroOffset, 0], s$12);
        }
      }
    }
  } else {
    return finalizeFloat(c$1.invalid, c$1.expecting, c$1.int, c$1.float, Dartea_parser.$$Parser$consumeBase(10, s$12.offset, s$12.src), s$12);
  }
});
const $$int = (expecting$3, invalid$2) => number({ int: Result.Ok(Basics.identity), hex: Result.Err(invalid$2), octal: Result.Err(invalid$2), binary: Result.Err(invalid$2), float: Result.Err(invalid$2), invalid: invalid$2, expecting: expecting$3 });
const $$float = (expecting$4, invalid$3) => number({ int: Result.Ok(Basics.toFloat), hex: Result.Err(invalid$3), octal: Result.Err(invalid$3), binary: Result.Err(invalid$3), float: Result.Ok(Basics.identity), invalid: invalid$3, expecting: expecting$4 });
const end = x$14 => Parser(s$13 => {
  if ($$String.length(s$13.src) === s$13.offset) {
    return Good(false, null, s$13);
  } else {
    return Bad(false, fromState(s$13, x$14));
  }
});
const mapChompedString = (func$2, $p1$3) => {
  const parse$6 = $p1$3._0;
  return Parser(s0$6 => {
  const $s27 = parse$6(s0$6);
  switch ($s27.TAG) {
    case "Bad":
      const p$6 = $s27._0;
      const x$15 = $s27._1;
      return Bad(p$6, x$15);
    case "Good":
      const p$7 = $s27._0;
      const a$6 = $s27._1;
      const s1$5 = $s27._2;
      return Good(p$7, Dartea_runtime.$$apply2(func$2, $$String.slice(s0$6.offset, s1$5.offset, s0$6.src), a$6), s1$5);
  }
});
};
const getChompedString = parser => mapChompedString(Basics.always, parser);
const chompIf = (isGood, expecting$5) => Parser(s$14 => {
  const newOffset$4 = Dartea_parser.$$Parser$isSubChar(isGood, s$14.offset, s$14.src);
  if (newOffset$4 === Basics.negate(1)) {
    return Bad(false, fromState(s$14, expecting$5));
  } else {
    if (newOffset$4 === Basics.negate(2)) {
      return Good(true, null, { src: s$14.src, offset: s$14.offset + 1, indent: s$14.indent, context: s$14.context, row: s$14.row + 1, col: 1 });
    } else {
      return Good(true, null, { src: s$14.src, offset: newOffset$4, indent: s$14.indent, context: s$14.context, row: s$14.row, col: s$14.col + 1 });
    }
  }
});
const chompWhileHelp = (isGood$1, offset$2, row$1, col$1, s0$7) => {
  while (true) {
    const newOffset$5 = Dartea_parser.$$Parser$isSubChar(isGood$1, offset$2, s0$7.src);
    if (newOffset$5 === Basics.negate(1)) {
      return Good(s0$7.offset < offset$2, null, { src: s0$7.src, offset: offset$2, indent: s0$7.indent, context: s0$7.context, row: row$1, col: col$1 });
    } else {
      if (newOffset$5 === Basics.negate(2)) {
        const $s33 = isGood$1;
        const $s34 = offset$2 + 1;
        const $s35 = row$1 + 1;
        const $s36 = 1;
        const $s37 = s0$7;
        isGood$1 = $s33;
        offset$2 = $s34;
        row$1 = $s35;
        col$1 = $s36;
        s0$7 = $s37;
        continue;
      } else {
        const $s28 = isGood$1;
        const $s29 = newOffset$5;
        const $s30 = row$1;
        const $s31 = col$1 + 1;
        const $s32 = s0$7;
        isGood$1 = $s28;
        offset$2 = $s29;
        row$1 = $s30;
        col$1 = $s31;
        s0$7 = $s32;
        continue;
      }
    }
  }
};
const chompWhile = isGood$2 => Parser(s$15 => chompWhileHelp(isGood$2, s$15.offset, s$15.row, s$15.col, s$15));
const chompUntil = $p0$4 => {
  const str$1 = $p0$4._0;
  const expecting$6 = $p0$4._1;
  return Parser(s$16 => {
  const $s38 = Dartea_parser.$$Parser$findSubString(str$1, s$16.offset, s$16.row, s$16.col, s$16.src);
  const newOffset$6 = $s38[0];
  const newRow$2 = $s38[1];
  const newCol$2 = $s38[2];
  if (newOffset$6 === Basics.negate(1)) {
    return Bad(false, fromInfo(newRow$2, newCol$2, expecting$6, s$16.context));
  } else {
    return Good(s$16.offset < newOffset$6, null, { src: s$16.src, offset: newOffset$6, indent: s$16.indent, context: s$16.context, row: newRow$2, col: newCol$2 });
  }
});
};
const chompUntilEndOr = str$2 => Parser(s$17 => {
  const $s39 = Dartea_parser.$$Parser$findSubString(str$2, s$17.offset, s$17.row, s$17.col, s$17.src);
  const newOffset$7 = $s39[0];
  const newRow$3 = $s39[1];
  const newCol$3 = $s39[2];
  const adjustedOffset = (newOffset$7 < 0) ? $$String.length(s$17.src) : newOffset$7;
  return Good(s$17.offset < adjustedOffset, null, { src: s$17.src, offset: adjustedOffset, indent: s$17.indent, context: s$17.context, row: newRow$3, col: newCol$3 });
});
const Located = ($a0$1, $a1$1, $a2$1) => ({ row: $a0$1, col: $a1$1, context: $a2$1 });
const changeContext = (newContext, s$18) => ({ src: s$18.src, offset: s$18.offset, indent: s$18.indent, context: newContext, row: s$18.row, col: s$18.col });
const inContext = (context$1, $p1$4) => {
  const parse$7 = $p1$4._0;
  return Parser(s0$8 => {
  const $s40 = parse$7(changeContext({ hd: Located(s0$8.row, s0$8.col, context$1), tl: s0$8.context }, s0$8));
  switch ($s40.TAG) {
    case "Good":
      const p$8 = $s40._0;
      const a$7 = $s40._1;
      const s1$6 = $s40._2;
      return Good(p$8, a$7, changeContext(s0$8.context, s1$6));
    case "Bad":
      const step$3 = $s40;
      return step$3;
  }
});
};
const getIndent = Parser(s$19 => Good(false, s$19.indent, s$19));
const changeIndent = (newIndent, s$20) => ({ src: s$20.src, offset: s$20.offset, indent: newIndent, context: s$20.context, row: s$20.row, col: s$20.col });
const withIndent = (newIndent$1, $p1$5) => {
  const parse$8 = $p1$5._0;
  return Parser(s0$9 => {
  const $s41 = parse$8(changeIndent(newIndent$1, s0$9));
  switch ($s41.TAG) {
    case "Good":
      const p$9 = $s41._0;
      const a$8 = $s41._1;
      const s1$7 = $s41._2;
      return Good(p$9, a$8, changeIndent(s0$9.indent, s1$7));
    case "Bad":
      const p$10 = $s41._0;
      const x$16 = $s41._1;
      return Bad(p$10, x$16);
  }
});
};
const getPosition = Parser(s$21 => Good(false, [s$21.row, s$21.col], s$21));
const getRow = Parser(s$22 => Good(false, s$22.row, s$22));
const getCol = Parser(s$23 => Good(false, s$23.col, s$23));
const getOffset = Parser(s$24 => Good(false, s$24.offset, s$24));
const getSource = Parser(s$25 => Good(false, s$25.src, s$25));
const varHelp = (isGood$3, offset$3, row$2, col$2, src$3, indent, context$2) => {
  while (true) {
    const newOffset$8 = Dartea_parser.$$Parser$isSubChar(isGood$3, offset$3, src$3);
    if (newOffset$8 === Basics.negate(1)) {
      return { src: src$3, offset: offset$3, indent: indent, context: context$2, row: row$2, col: col$2 };
    } else {
      if (newOffset$8 === Basics.negate(2)) {
        const $s49 = isGood$3;
        const $s50 = offset$3 + 1;
        const $s51 = row$2 + 1;
        const $s52 = 1;
        const $s53 = src$3;
        const $s54 = indent;
        const $s55 = context$2;
        isGood$3 = $s49;
        offset$3 = $s50;
        row$2 = $s51;
        col$2 = $s52;
        src$3 = $s53;
        indent = $s54;
        context$2 = $s55;
        continue;
      } else {
        const $s42 = isGood$3;
        const $s43 = newOffset$8;
        const $s44 = row$2;
        const $s45 = col$2 + 1;
        const $s46 = src$3;
        const $s47 = indent;
        const $s48 = context$2;
        isGood$3 = $s42;
        offset$3 = $s43;
        row$2 = $s44;
        col$2 = $s45;
        src$3 = $s46;
        indent = $s47;
        context$2 = $s48;
        continue;
      }
    }
  }
};
const variable = i => Parser(s$26 => {
  const firstOffset = Dartea_parser.$$Parser$isSubChar(i.start, s$26.offset, s$26.src);
  if (firstOffset === Basics.negate(1)) {
    return Bad(false, fromState(s$26, i.expecting));
  } else {
    const s1$8 = (firstOffset === Basics.negate(2)) ? varHelp(i.inner, s$26.offset + 1, s$26.row + 1, 1, s$26.src, s$26.indent, s$26.context) : varHelp(i.inner, firstOffset, s$26.row, s$26.col + 1, s$26.src, s$26.indent, s$26.context);
    const name = $$String.slice(s$26.offset, s1$8.offset, s$26.src);
    if ($$Set.member(name, i.reserved)) {
      return Bad(false, fromState(s$26, i.expecting));
    } else {
      return Good(true, name, s1$8);
    }
  }
});
const revAlways = ($p0$5, b$2) => b$2;
const sequenceEndForbidden = (ender, ws, parseItem, sep, revItems) => {
  const parsers$2 = { hd: map2(revAlways, sep, map2(revAlways, ws, map(item => Loop({ hd: item, tl: revItems }), parseItem))), tl: { hd: map($p0$6 => Done(List.reverse(revItems)), ender), tl: 0 } };
  return map2(revAlways, ws, Parser(s$27 => oneOfHelp(s$27, Empty, parsers$2)));
};
const sequenceEndMandatory = (ws$1, parseItem$1, sep$1, revItems$1) => {
  const a$9 = null;
  const parsers$3 = { hd: map(item$1 => Loop({ hd: item$1, tl: revItems$1 }), map2(Basics.always, parseItem$1, map2(Basics.always, ws$1, map2(Basics.always, sep$1, ws$1)))), tl: { hd: map($p0$7 => Done(List.reverse(revItems$1)), Parser(s$28 => Good(false, a$9, s$28))), tl: 0 } };
  return Parser(s$29 => oneOfHelp(s$29, Empty, parsers$3));
};
const sequenceEndOptional = (ender$1, ws$2, parseItem$2, sep$2, revItems$2) => {
  const parseEnd = map($p0$8 => Done(List.reverse(revItems$2)), ender$1);
  const parsers$5 = { hd: map(item$2 => Loop({ hd: item$2, tl: revItems$2 }), parseItem$2), tl: { hd: parseEnd, tl: 0 } };
  const parsers$4 = { hd: map2(revAlways, sep$2, map2(revAlways, ws$2, Parser(s$30 => oneOfHelp(s$30, Empty, parsers$5)))), tl: { hd: parseEnd, tl: 0 } };
  return map2(revAlways, ws$2, Parser(s$31 => oneOfHelp(s$31, Empty, parsers$4)));
};
const sequenceEnd = (ender$2, ws$3, parseItem$3, sep$3, trailing) => {
  const chompRest = item$3 => {
  switch (trailing) {
    case "Forbidden":
      return loop({ hd: item$3, tl: 0 }, $s56 => sequenceEndForbidden(ender$2, ws$3, parseItem$3, sep$3, $s56));
    case "Optional":
      return loop({ hd: item$3, tl: 0 }, $s57 => sequenceEndOptional(ender$2, ws$3, parseItem$3, sep$3, $s57));
    case "Mandatory":
      return map2(Basics.always, map2(revAlways, ws$3, map2(revAlways, sep$3, map2(revAlways, ws$3, loop({ hd: item$3, tl: 0 }, $s58 => sequenceEndMandatory(ws$3, parseItem$3, sep$3, $s58))))), ender$2);
  }
};
  const parsers$6 = { hd: andThen(chompRest, parseItem$3), tl: { hd: map($p0$9 => 0, ender$2), tl: 0 } };
  return Parser(s$32 => oneOfHelp(s$32, Empty, parsers$6));
};
const sequence = i$1 => map2(revAlways, token(i$1.start), map2(revAlways, i$1.spaces, sequenceEnd(token(i$1.end), i$1.spaces, i$1.item, token(i$1.separator), i$1.trailing)));
const spaces = chompWhile(c$2 => (c$2 === " ") || ((c$2 === "\n") || (c$2 === "\r")));
const lineComment = start => map2(Basics.always, token(start), chompUntilEndOr("\n"));
const isChar = $$char => true;
const nestableHelp = (isNotRelevant, open, close, expectingClose, nestLevel) => {
  const parsers$7 = { hd: (nestLevel === 1) ? close : andThen($p0$10 => nestableHelp(isNotRelevant, open, close, expectingClose, nestLevel - 1), close), tl: { hd: andThen($p0$11 => nestableHelp(isNotRelevant, open, close, expectingClose, nestLevel + 1), open), tl: { hd: andThen($p0$12 => nestableHelp(isNotRelevant, open, close, expectingClose, nestLevel), chompIf(isChar, expectingClose)), tl: 0 } } };
  return map2(revAlways, chompWhile(isNotRelevant), Parser(s$33 => oneOfHelp(s$33, Empty, parsers$7)));
};
const nestableComment = ($p0$13, $p1$6) => {
  const open$1 = $p0$13;
  const oStr = $p0$13._0;
  const oX = $p0$13._1;
  const close$1 = $p1$6;
  const cStr = $p1$6._0;
  const cX = $p1$6._1;
  const $s59 = $$String.uncons(oStr);
  if ($s59 === "Nothing") {
    return problem(oX);
  } else {
    const openChar = $s59._0[0];
    const $s60 = $$String.uncons(cStr);
    if ($s60 === "Nothing") {
      return problem(cX);
    } else {
      const closeChar = $s60._0[0];
      const isNotRelevant$1 = $$char$1 => ($$char$1 !== openChar) && ($$char$1 !== closeChar);
      const chompOpen = token(open$1);
      return map2(Basics.always, chompOpen, nestableHelp(isNotRelevant$1, chompOpen, token(close$1), cX, 1));
    }
  }
};
const multiComment = (open$2, close$2, nestable) => {
  switch (nestable) {
    case "NotNestable":
      return map2(Basics.always, token(open$2), chompUntil(close$2));
    case "Nestable":
      return nestableComment(open$2, close$2);
  }
};
export { DeadEnd, Done, Forbidden, Loop, Mandatory, Nestable, NotNestable, Optional, Token, andThen, backtrackable, chompIf, chompUntil, chompUntilEndOr, chompWhile, commit, end, $$float, getChompedString, getCol, getIndent, getOffset, getPosition, getRow, getSource, inContext, $$int, keyword, lazy, lineComment, loop, map, mapChompedString, multiComment, number, oneOf, problem, run, sequence, spaces, succeed, symbol, token, variable, withIndent, $$pipe$dot, $$pipe$eq };
