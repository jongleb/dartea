// Compiled by dartea, an independent compiler. Not affiliated with or
// endorsed by the Elm project.
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
const decodeValue = (eta1, eta2) => {
  const decode = eta1._0;
  return decode(eta2);
};
const decodeString = (decoder, text) => {
  if (Dartea_json.$$Json$isValid(text)) {
    const decode$1 = decoder._0;
    return decode$1(Dartea_json.$$Json$unsafeParse(text));
  } else {
    return Result.Err(Failure("This is not valid JSON!", Dartea_json.$$Json$unsafeParse("null")));
  }
};
const succeed = given => Decoder($p0 => Result.Ok(given));
const fail = message => Decoder(input => Result.Err(Failure(message, input)));
const value = Decoder(input$1 => Result.Ok(input$1));
const primitive = (fits, take, expected) => Decoder(input$2 => {
  if (fits(input$2)) {
    return Result.Ok(Dartea_runtime.$$apply1(take, input$2));
  } else {
    return Result.Err(Failure("Expecting " + expected, input$2));
  }
});
const string = primitive(Dartea_json.$$Json$isString, Dartea_json.$$Json$identity, "a STRING");
const bool = primitive(Dartea_json.$$Json$isBool, Dartea_json.$$Json$identity, "a BOOL");
const $$int = primitive(Dartea_json.$$Json$isInt, Dartea_json.$$Json$identity, "an INT");
const $$float = primitive(Dartea_json.$$Json$isNumber, Dartea_json.$$Json$identity, "a FLOAT");
const $$null = given$1 => Decoder(input$3 => {
  if (Dartea_json.$$Json$isNull(input$3)) {
    return Result.Ok(given$1);
  } else {
    return Result.Err(Failure("Expecting null", input$3));
  }
});
const map = (change, decoder$1) => Decoder(input$4 => {
  let $s1;
  const decode$2 = decoder$1._0;
  $s1 = decode$2(input$4);
  return Result.map(change, $s1);
});
const map2 = (combine, first, second) => Decoder(input$5 => {
  let $s2;
  const decode$3 = first._0;
  $s2 = decode$3(input$5);
  switch ($s2.TAG) {
    case "Ok":
      const one = $s2._0;
      let $s3;
      const decode$4 = second._0;
      $s3 = decode$4(input$5);
      return Result.map(Dartea_runtime.$$apply1(combine, one), $s3);
    case "Err":
      const error = $s2._0;
      return Result.Err(error);
  }
});
const map3 = (combine$1, first$1, second$1, third) => map2((apply, given$2) => Dartea_runtime.$$apply1(apply, given$2), map2(combine$1, first$1, second$1), third);
const andThen = (next, decoder$2) => Decoder(input$6 => {
  let $s4;
  const decode$5 = decoder$2._0;
  $s4 = decode$5(input$6);
  switch ($s4.TAG) {
    case "Ok":
      const given$3 = $s4._0;
      const $s5 = next(given$3);
      const decode$6 = $s5._0;
      return decode$6(input$6);
    case "Err":
      const error$1 = $s4._0;
      return Result.Err(error$1);
  }
});
const field = (name, decoder$3) => Decoder(input$7 => {
  if (Dartea_json.$$Json$isObject(input$7) && Dartea_json.$$Json$hasField(name, input$7)) {
    let $s6;
    const decode$7 = decoder$3._0;
    $s6 = decode$7(Dartea_json.$$Json$unsafeField(name, input$7));
    switch ($s6.TAG) {
      case "Ok":
        const given$4 = $s6._0;
        return Result.Ok(given$4);
      case "Err":
        const error$2 = $s6._0;
        return Result.Err(Field(name, error$2));
    }
  } else {
    return Result.Err(Failure("Expecting an OBJECT with a field named `" + (name + "`"), input$7));
  }
});
const at = (names, decoder$4) => List.foldr(field, decoder$4, names);
const index = (wanted, decoder$5) => Decoder(input$8 => {
  if (Dartea_json.$$Json$isArray(input$8) && (wanted < Dartea_json.$$Json$length(input$8))) {
    let $s7;
    const decode$8 = decoder$5._0;
    $s7 = decode$8(Dartea_json.$$Json$unsafeIndex(wanted, input$8));
    switch ($s7.TAG) {
      case "Ok":
        const given$5 = $s7._0;
        return Result.Ok(given$5);
      case "Err":
        const error$3 = $s7._0;
        return Result.Err(Index(wanted, error$3));
    }
  } else {
    return Result.Err(Failure("Expecting a longer ARRAY", input$8));
  }
});
const items = (decoder$6, input$9, at_, collected) => {
  while (true) {
    if (at_ < 0) {
      return Result.Ok(collected);
    } else {
      let $s8;
      const decode$9 = decoder$6._0;
      $s8 = decode$9(Dartea_json.$$Json$unsafeIndex(at_, input$9));
      switch ($s8.TAG) {
        case "Ok":
          const given$6 = $s8._0;
          const $s9 = decoder$6;
          const $s10 = input$9;
          const $s11 = at_ - 1;
          const $s12 = { hd: given$6, tl: collected };
          decoder$6 = $s9;
          input$9 = $s10;
          at_ = $s11;
          collected = $s12;
          continue;
        case "Err":
          const error$4 = $s8._0;
          return Result.Err(Index(at_, error$4));
      }
    }
  }
};
const list = decoder$7 => Decoder(input$10 => {
  if (Dartea_json.$$Json$isArray(input$10)) {
    return items(decoder$7, input$10, Dartea_json.$$Json$length(input$10) - 1, 0);
  } else {
    return Result.Err(Failure("Expecting a LIST", input$10));
  }
});
const tried = (decoders, input$11, failures) => {
  while (true) {
    if (decoders === 0) {
      return Result.Err(OneOf(List.reverse(failures)));
    } else {
      const decoder$8 = decoders.hd;
      const rest = decoders.tl;
      let $s13;
      const decode$10 = decoder$8._0;
      $s13 = decode$10(input$11);
      switch ($s13.TAG) {
        case "Ok":
          const given$7 = $s13._0;
          return Result.Ok(given$7);
        case "Err":
          const error$5 = $s13._0;
          const $s14 = rest;
          const $s15 = input$11;
          const $s16 = { hd: error$5, tl: failures };
          decoders = $s14;
          input$11 = $s15;
          failures = $s16;
          continue;
      }
    }
  }
};
const oneOf = decoders$1 => Decoder(input$12 => tried(decoders$1, input$12, 0));
const maybe = decoder$9 => Decoder(input$13 => {
  let $s17;
  const decode$11 = decoder$9._0;
  $s17 = decode$11(input$13);
  switch ($s17.TAG) {
    case "Ok":
      const given$8 = $s17._0;
      return Result.Ok(Maybe.Just(given$8));
    case "Err":
      return Result.Ok(Maybe.Nothing);
  }
});
const nullable = decoder$10 => {
  const decoders$2 = { hd: $$null(Maybe.Nothing), tl: { hd: map(Maybe.Just, decoder$10), tl: 0 } };
  return Decoder(input$14 => tried(decoders$2, input$14, 0));
};
const errorToString = error$6 => {
  switch (error$6.TAG) {
    case "Field":
      const name$1 = error$6._0;
      const inner = error$6._1;
      return "At field `" + (name$1 + ("`:\n" + ("    " + errorToString(inner))));
    case "Index":
      const spot = error$6._0;
      const inner$1 = error$6._1;
      return "At index " + ($$String.fromInt(spot) + (":\n" + ("    " + errorToString(inner$1))));
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
const map4 = (func, one$1, other, third$1, fourth) => andThen(a => map3(Dartea_runtime.$$apply1(func, a), other, third$1, fourth), one$1);
const map5 = (func$1, one$2, other$1, third$2, fourth$1, fifth) => andThen(a$1 => map4(Dartea_runtime.$$apply1(func$1, a$1), other$1, third$2, fourth$1, fifth), one$2);
const map6 = (func$2, one$3, other$2, third$3, fourth$2, fifth$1, sixth) => andThen(a$2 => map5(Dartea_runtime.$$apply1(func$2, a$2), other$2, third$3, fourth$2, fifth$1, sixth), one$3);
const map7 = (func$3, one$4, other$3, third$4, fourth$3, fifth$2, sixth$1, seventh) => andThen(a$3 => map6(Dartea_runtime.$$apply1(func$3, a$3), other$3, third$4, fourth$3, fifth$2, sixth$1, seventh), one$4);
const map8 = (func$4, one$5, other$4, third$5, fourth$4, fifth$3, sixth$2, seventh$1, eighth) => andThen(a$4 => map7(Dartea_runtime.$$apply1(func$4, a$4), other$4, third$5, fourth$4, fifth$3, sixth$2, seventh$1, eighth), one$5);
const lazy = thunk => {
  const given$10 = null;
  return andThen(thunk, Decoder($p0$1 => Result.Ok(given$10)));
};
const oneOrMoreHelp = (func$5, values) => {
  if (values === 0) {
    return Decoder(input$15 => Result.Err(Failure("a ARRAY with at least ONE element", input$15)));
  } else {
    const first$2 = values.hd;
    const rest$1 = values.tl;
    const given$11 = Dartea_runtime.$$apply2(func$5, first$2, rest$1);
    return Decoder($p0$2 => Result.Ok(given$11));
  }
};
const oneOrMore = (func$6, decoder$11) => andThen($s18 => oneOrMoreHelp(func$6, $s18), list(decoder$11));
export { Failure, Field, Index, OneOf, andThen, at, bool, decodeString, decodeValue, errorToString, fail, field, $$float, index, $$int, lazy, list, map, map2, map3, map4, map5, map6, map7, map8, maybe, $$null, nullable, oneOf, oneOrMore, string, succeed, value };
