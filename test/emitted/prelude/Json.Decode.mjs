// Compiled by dartea, an independent compiler. Not affiliated with or
// endorsed by the Elm project.
// Contains material derived from elm/json,
// Copyright 2014-present Evan Czaplicki, under the BSD 3-Clause License.
// dartea's LICENSE carries the full text.
import * as Dartea_runtime from "./Dartea_runtime.mjs";
import * as Dartea_json from "./Dartea_json.mjs";
import * as Json$Encode from "./Json.Encode.mjs";
import * as List from "./List.mjs";
import * as Maybe from "./Maybe.mjs";
import * as Result from "./Result.mjs";
import * as $$String from "./String.mjs";
const Field = (_0, _1) => ({ TAG: "Field", _0: _0, _1: _1 });
const Index = (_0, _1) => ({ TAG: "Index", _0: _0, _1: _1 });
const OneOf = _0 => ({ TAG: "OneOf", _0: _0 });
const Failure = (_0, _1) => ({ TAG: "Failure", _0: _0, _1: _1 });
const Decoder = _0 => ({ _0: _0 });
const isString = Dartea_json.$$Json$isString;
const isBool = Dartea_json.$$Json$isBool;
const isNumber = Dartea_json.$$Json$isNumber;
const isWhole = Dartea_json.$$Json$isInt;
const isNull = Dartea_json.$$Json$isNull;
const isArray = Dartea_json.$$Json$isArray;
const isObject = Dartea_json.$$Json$isObject;
const hasField = Dartea_json.$$Json$hasField;
const rawField = Dartea_json.$$Json$unsafeField;
const rawLength = Dartea_json.$$Json$length;
const rawIndex = Dartea_json.$$Json$unsafeIndex;
const asString = Dartea_json.$$Json$identity;
const asBool = Dartea_json.$$Json$identity;
const asInt = Dartea_json.$$Json$identity;
const asFloat = Dartea_json.$$Json$identity;
const isValidJson = Dartea_json.$$Json$isValid;
const parsed = Dartea_json.$$Json$unsafeParse;
const run = (decoder, input) => {
  const decode = decoder._0;
  return decode(input);
};
const decodeValue = (eta1, eta2) => {
  const decode$1 = eta1._0;
  return decode$1(eta2);
};
const decodeString = (decoder$1, text) => {
  if (isValidJson(text)) {
    return run(decoder$1, parsed(text));
  } else {
    return Result.Err(Failure("This is not valid JSON!", parsed("null")));
  }
};
const succeed = given => Decoder($p0 => Result.Ok(given));
const fail = message => Decoder(input$1 => Result.Err(Failure(message, input$1)));
const value = Decoder(input$2 => Result.Ok(input$2));
const primitive = (fits, take, expected) => Decoder(input$3 => {
  if (fits(input$3)) {
    return Result.Ok(Dartea_runtime.$$curry(take, [input$3]));
  } else {
    return Result.Err(Failure("Expecting " + expected, input$3));
  }
});
const string = primitive(isString, asString, "a STRING");
const bool = primitive(isBool, asBool, "a BOOL");
const $$int = primitive(isWhole, asInt, "an INT");
const $$float = primitive(isNumber, asFloat, "a FLOAT");
const $$null = given$1 => Decoder(input$4 => {
  if (isNull(input$4)) {
    return Result.Ok(given$1);
  } else {
    return Result.Err(Failure("Expecting null", input$4));
  }
});
const map = (change, decoder$2) => Decoder(input$5 => Result.map(change, run(decoder$2, input$5)));
const map2 = (combine, first, second) => Decoder(input$6 => {
  const $s1 = run(first, input$6);
  switch ($s1.TAG) {
    case "Ok":
      const one = $s1._0;
      return Result.map(Dartea_runtime.$$curry(combine, [one]), run(second, input$6));
    case "Err":
      const error = $s1._0;
      return Result.Err(error);
  }
});
const map3 = (combine$1, first$1, second$1, third) => map2((apply, given$2) => Dartea_runtime.$$curry(apply, [given$2]), map2(combine$1, first$1, second$1), third);
const andThen = (next, decoder$3) => Decoder(input$7 => {
  const $s2 = run(decoder$3, input$7);
  switch ($s2.TAG) {
    case "Ok":
      const given$3 = $s2._0;
      return run(next(given$3), input$7);
    case "Err":
      const error$1 = $s2._0;
      return Result.Err(error$1);
  }
});
const field = (name, decoder$4) => Decoder(input$8 => {
  if (isObject(input$8) && hasField(name, input$8)) {
    const $s3 = run(decoder$4, rawField(name, input$8));
    switch ($s3.TAG) {
      case "Ok":
        const given$4 = $s3._0;
        return Result.Ok(given$4);
      case "Err":
        const error$2 = $s3._0;
        return Result.Err(Field(name, error$2));
    }
  } else {
    return Result.Err(Failure("Expecting an OBJECT with a field named `" + (name + "`"), input$8));
  }
});
const at = (names, decoder$5) => List.foldr(field, decoder$5, names);
const index = (wanted, decoder$6) => Decoder(input$9 => {
  if (isArray(input$9) && (wanted < rawLength(input$9))) {
    const $s4 = run(decoder$6, rawIndex(wanted, input$9));
    switch ($s4.TAG) {
      case "Ok":
        const given$5 = $s4._0;
        return Result.Ok(given$5);
      case "Err":
        const error$3 = $s4._0;
        return Result.Err(Index(wanted, error$3));
    }
  } else {
    return Result.Err(Failure("Expecting a longer ARRAY", input$9));
  }
});
const items = (decoder$7, input$10, at_, collected) => {
  while (true) {
    if (at_ < 0) {
      return Result.Ok(collected);
    } else {
      const $s5 = run(decoder$7, rawIndex(at_, input$10));
      switch ($s5.TAG) {
        case "Ok":
          const given$6 = $s5._0;
          const $s6 = decoder$7;
          const $s7 = input$10;
          const $s8 = at_ - 1;
          const $s9 = { hd: given$6, tl: collected };
          decoder$7 = $s6;
          input$10 = $s7;
          at_ = $s8;
          collected = $s9;
          continue;
        case "Err":
          const error$4 = $s5._0;
          return Result.Err(Index(at_, error$4));
      }
    }
  }
};
const list = decoder$8 => Decoder(input$11 => {
  if (isArray(input$11)) {
    return items(decoder$8, input$11, rawLength(input$11) - 1, 0);
  } else {
    return Result.Err(Failure("Expecting a LIST", input$11));
  }
});
const tried = (decoders, input$12, failures) => {
  while (true) {
    if (decoders === 0) {
      return Result.Err(OneOf(List.reverse(failures)));
    } else {
      const decoder$9 = decoders.hd;
      const rest = decoders.tl;
      const $s10 = run(decoder$9, input$12);
      switch ($s10.TAG) {
        case "Ok":
          const given$7 = $s10._0;
          return Result.Ok(given$7);
        case "Err":
          const error$5 = $s10._0;
          const $s11 = rest;
          const $s12 = input$12;
          const $s13 = { hd: error$5, tl: failures };
          decoders = $s11;
          input$12 = $s12;
          failures = $s13;
          continue;
      }
    }
  }
};
const oneOf = decoders$1 => Decoder(input$13 => tried(decoders$1, input$13, 0));
const maybe = decoder$10 => Decoder(input$14 => {
  const $s14 = run(decoder$10, input$14);
  switch ($s14.TAG) {
    case "Ok":
      const given$8 = $s14._0;
      return Result.Ok(Maybe.Just(given$8));
    case "Err":
      return Result.Ok(Maybe.Nothing);
  }
});
const nullable = decoder$11 => {
  const decoders$2 = { hd: $$null(Maybe.Nothing), tl: { hd: map(Maybe.Just, decoder$11), tl: 0 } };
  return Decoder(input$15 => tried(decoders$2, input$15, 0));
};
const errorToString = error$6 => {
  switch (error$6.TAG) {
    case "Field":
      const name$1 = error$6._0;
      const inner = error$6._1;
      const text$1 = errorToString(inner);
      return "At field `" + (name$1 + ("`:\n" + ("    " + text$1)));
    case "Index":
      const spot = error$6._0;
      const inner$1 = error$6._1;
      const text$2 = errorToString(inner$1);
      return "At index " + ($$String.fromInt(spot) + (":\n" + ("    " + text$2)));
    case "OneOf":
      const failures$1 = error$6._0;
      if (failures$1 === 0) {
        return "Ran into a Json.Decode.oneOf with no possibilities!";
      } else {
        return "I ran into the following problems:\n" + $$String.join("\n", List.map(errorToString, failures$1));
      }
    case "Failure":
      const message$1 = error$6._0;
      const given$9 = error$6._1;
      return message$1 + (", but instead got: " + Json$Encode.encode(0, given$9));
  }
};
const map4 = (func, one$1, other, third$1, fourth) => andThen(a => map3(Dartea_runtime.$$curry(func, [a]), other, third$1, fourth), one$1);
const map5 = (func$1, one$2, other$1, third$2, fourth$1, fifth) => andThen(a$1 => map4(Dartea_runtime.$$curry(func$1, [a$1]), other$1, third$2, fourth$1, fifth), one$2);
const map6 = (func$2, one$3, other$2, third$3, fourth$2, fifth$1, sixth) => andThen(a$2 => map5(Dartea_runtime.$$curry(func$2, [a$2]), other$2, third$3, fourth$2, fifth$1, sixth), one$3);
const map7 = (func$3, one$4, other$3, third$4, fourth$3, fifth$2, sixth$1, seventh) => andThen(a$3 => map6(Dartea_runtime.$$curry(func$3, [a$3]), other$3, third$4, fourth$3, fifth$2, sixth$1, seventh), one$4);
const map8 = (func$4, one$5, other$4, third$5, fourth$4, fifth$3, sixth$2, seventh$1, eighth) => andThen(a$4 => map7(Dartea_runtime.$$curry(func$4, [a$4]), other$4, third$5, fourth$4, fifth$3, sixth$2, seventh$1, eighth), one$5);
const lazy = thunk => {
  const given$10 = null;
  return andThen(thunk, Decoder($p0$1 => Result.Ok(given$10)));
};
const oneOrMoreHelp = (func$5, values) => {
  if (values === 0) {
    return Decoder(input$16 => Result.Err(Failure("a ARRAY with at least ONE element", input$16)));
  } else {
    const first$2 = values.hd;
    const rest$1 = values.tl;
    const given$11 = Dartea_runtime.$$curry(func$5, [first$2, rest$1]);
    return Decoder($p0$2 => Result.Ok(given$11));
  }
};
const oneOrMore = (func$6, decoder$12) => andThen($s15 => oneOrMoreHelp(func$6, $s15), list(decoder$12));
export { Failure, Field, Index, OneOf, andThen, at, bool, decodeString, decodeValue, errorToString, fail, field, $$float, index, $$int, lazy, list, map, map2, map3, map4, map5, map6, map7, map8, maybe, $$null, nullable, oneOf, oneOrMore, string, succeed, value };
