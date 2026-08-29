// Compiled by dartea, an independent compiler. Not affiliated with or
// endorsed by the Elm project.
// Contains material derived from elm/json,
// Copyright 2014-present Evan Czaplicki, under the BSD 3-Clause License.
// dartea's LICENSE carries the full text.
import * as Dartea_json from "./Dartea_json.mjs";
import * as List from "./List.mjs";
import * as Tuple from "./Tuple.mjs";
const string = Dartea_json.$$Json$identity;
const $$int = Dartea_json.$$Json$identity;
const $$float = Dartea_json.$$Json$identity;
const bool = Dartea_json.$$Json$identity;
const $$null = Dartea_json.$$Json$unsafeParse("null");
const list = (encoder, items) => List.foldl((item, built) => Dartea_json.$$Json$pushed(encoder(item), built), Dartea_json.$$Json$emptyArray, items);
const object = pairs => List.foldl((pair, built$1) => Dartea_json.$$Json$withField(Tuple.first(pair), Tuple.second(pair), built$1), Dartea_json.$$Json$emptyObject, pairs);
const encode = Dartea_json.$$Json$stringify;
export { bool, encode, $$float, $$int, list, $$null, object, string };
