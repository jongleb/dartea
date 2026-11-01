import * as List from "./List.mjs";
import * as Parser$Advanced from "./Parser.Advanced.mjs";
import * as Result from "./Result.mjs";
const Forbidden = "Forbidden";
const Optional = "Optional";
const Mandatory = "Mandatory";
const Loop = _0 => ({ TAG: "Loop", _0: _0 });
const Done = _0 => ({ TAG: "Done", _0: _0 });
const Expecting = _0 => ({ TAG: "Expecting", _0: _0 });
const ExpectingInt = "ExpectingInt";
const ExpectingHex = "ExpectingHex";
const ExpectingOctal = "ExpectingOctal";
const ExpectingBinary = "ExpectingBinary";
const ExpectingFloat = "ExpectingFloat";
const ExpectingNumber = "ExpectingNumber";
const ExpectingVariable = "ExpectingVariable";
const ExpectingSymbol = _0 => ({ TAG: "ExpectingSymbol", _0: _0 });
const ExpectingKeyword = _0 => ({ TAG: "ExpectingKeyword", _0: _0 });
const ExpectingEnd = "ExpectingEnd";
const UnexpectedChar = "UnexpectedChar";
const Problem = _0 => ({ TAG: "Problem", _0: _0 });
const BadRepeat = "BadRepeat";
const NotNestable = "NotNestable";
const Nestable = "Nestable";
const $$pipe$eq = (eta1, eta2) => Parser$Advanced.$$pipe$eq(eta1, eta2);
const $$pipe$dot = (eta1$1, eta2$1) => Parser$Advanced.$$pipe$dot(eta1$1, eta2$1);
const DeadEnd = ($a0, $a1, $a2) => ({ row: $a0, col: $a1, problem: $a2 });
const problemToDeadEnd = p => DeadEnd(p.row, p.col, p.problem);
const run = (parser, source) => {
  const $s1 = Parser$Advanced.run(parser, source);
  switch ($s1.TAG) {
    case "Ok":
      const a = $s1._0;
      return Result.Ok(a);
    case "Err":
      const problems = $s1._0;
      return Result.Err(List.map(problemToDeadEnd, problems));
  }
};
const deadEndsToString = deadEnds => "TODO deadEndsToString";
const succeed = eta1$2 => Parser$Advanced.succeed(eta1$2);
const lazy = eta1$3 => Parser$Advanced.lazy(eta1$3);
const andThen = (eta1$4, eta2$2) => Parser$Advanced.andThen(eta1$4, eta2$2);
const problem = msg => Parser$Advanced.problem(Problem(msg));
const oneOf = eta1$5 => Parser$Advanced.oneOf(eta1$5);
const map = (eta1$6, eta2$3) => Parser$Advanced.map(eta1$6, eta2$3);
const backtrackable = eta1$7 => Parser$Advanced.backtrackable(eta1$7);
const commit = eta1$8 => Parser$Advanced.commit(eta1$8);
const token = str => Parser$Advanced.token(Parser$Advanced.Token(str, Expecting(str)));
const toAdvancedStep = step => {
  switch (step.TAG) {
    case "Loop":
      const s = step._0;
      return Parser$Advanced.Loop(s);
    case "Done":
      const a$1 = step._0;
      return Parser$Advanced.Done(a$1);
  }
};
const loop = (state, callback) => Parser$Advanced.loop(state, s$1 => Parser$Advanced.map(toAdvancedStep, callback(s$1)));
const $$int = Parser$Advanced.$$int(ExpectingInt, ExpectingInt);
const $$float = Parser$Advanced.$$float(ExpectingFloat, ExpectingFloat);
const number = i => Parser$Advanced.number({ int: Result.fromMaybe(ExpectingInt, i.int), hex: Result.fromMaybe(ExpectingHex, i.hex), octal: Result.fromMaybe(ExpectingOctal, i.octal), binary: Result.fromMaybe(ExpectingBinary, i.binary), float: Result.fromMaybe(ExpectingFloat, i.float), invalid: ExpectingNumber, expecting: ExpectingNumber });
const symbol = str$1 => Parser$Advanced.symbol(Parser$Advanced.Token(str$1, ExpectingSymbol(str$1)));
const keyword = kwd => Parser$Advanced.keyword(Parser$Advanced.Token(kwd, ExpectingKeyword(kwd)));
const end = Parser$Advanced.end(ExpectingEnd);
const getChompedString = eta1$9 => Parser$Advanced.getChompedString(eta1$9);
const mapChompedString = (eta1$10, eta2$4) => Parser$Advanced.mapChompedString(eta1$10, eta2$4);
const chompIf = isGood => Parser$Advanced.chompIf(isGood, UnexpectedChar);
const chompWhile = eta1$11 => Parser$Advanced.chompWhile(eta1$11);
const chompUntil = str$2 => Parser$Advanced.chompUntil(Parser$Advanced.Token(str$2, Expecting(str$2)));
const chompUntilEndOr = eta1$12 => Parser$Advanced.chompUntilEndOr(eta1$12);
const withIndent = (eta1$13, eta2$5) => Parser$Advanced.withIndent(eta1$13, eta2$5);
const getIndent = Parser$Advanced.getIndent;
const getPosition = Parser$Advanced.getPosition;
const getRow = Parser$Advanced.getRow;
const getCol = Parser$Advanced.getCol;
const getOffset = Parser$Advanced.getOffset;
const getSource = Parser$Advanced.getSource;
const variable = i$1 => Parser$Advanced.variable({ start: i$1.start, inner: i$1.inner, reserved: i$1.reserved, expecting: ExpectingVariable });
const toAdvancedTrailing = trailing => {
  switch (trailing) {
    case "Forbidden":
      return Parser$Advanced.Forbidden;
    case "Optional":
      return Parser$Advanced.Optional;
    case "Mandatory":
      return Parser$Advanced.Mandatory;
  }
};
const sequence = i$2 => {
  const str$3 = i$2.start;
  const str$4 = i$2.separator;
  const str$5 = i$2.end;
  return Parser$Advanced.sequence({ start: Parser$Advanced.Token(str$3, Expecting(str$3)), separator: Parser$Advanced.Token(str$4, Expecting(str$4)), end: Parser$Advanced.Token(str$5, Expecting(str$5)), spaces: i$2.spaces, item: i$2.item, trailing: toAdvancedTrailing(i$2.trailing) });
};
const spaces = Parser$Advanced.spaces;
const lineComment = str$6 => Parser$Advanced.lineComment(Parser$Advanced.Token(str$6, Expecting(str$6)));
const multiComment = (open, close, nestable) => {
  let $s2;
  if (nestable === "NotNestable") {
    $s2 = Parser$Advanced.NotNestable;
  } else {
    $s2 = Parser$Advanced.Nestable;
  }
  return Parser$Advanced.multiComment(Parser$Advanced.Token(open, Expecting(open)), Parser$Advanced.Token(close, Expecting(close)), $s2);
};
export { BadRepeat, DeadEnd, Done, Expecting, ExpectingBinary, ExpectingEnd, ExpectingFloat, ExpectingHex, ExpectingInt, ExpectingKeyword, ExpectingNumber, ExpectingOctal, ExpectingSymbol, ExpectingVariable, Forbidden, Loop, Mandatory, Nestable, NotNestable, Optional, Problem, UnexpectedChar, andThen, backtrackable, chompIf, chompUntil, chompUntilEndOr, chompWhile, commit, deadEndsToString, end, $$float, getChompedString, getCol, getIndent, getOffset, getPosition, getRow, getSource, $$int, keyword, lazy, lineComment, loop, map, mapChompedString, multiComment, number, oneOf, problem, run, sequence, spaces, succeed, symbol, token, variable, withIndent, $$pipe$dot, $$pipe$eq };
