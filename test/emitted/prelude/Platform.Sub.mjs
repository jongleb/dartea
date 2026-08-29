// Compiled by dartea, an independent compiler. Not affiliated with or
// endorsed by the Elm project.
// Contains material derived from elm/core,
// Copyright 2014-present Evan Czaplicki, under the BSD 3-Clause License.
// dartea's LICENSE carries the full text.
import * as Dartea_runtime from "./Dartea_runtime.mjs";
import * as List from "./List.mjs";
const None = "None";
const Batch = _0 => ({ TAG: "Batch", _0: _0 });
const Listen = (_0, _1) => ({ TAG: "Listen", _0: _0, _1: _1 });
const none = None;
const batch = eta1 => Batch(eta1);
const map = (tagger, subscription) => {
  if (subscription === "None") {
    return None;
  } else {
    if (subscription.TAG === "Batch") {
      const subscriptions = subscription._0;
      return Batch(List.map($s4 => map(tagger, $s4), subscriptions));
    } else {
      const key = subscription._0;
      const watch = subscription._1;
      return Listen(key, ($s2, $s3) => (dispatch => $s1 => watch(value => dispatch(Dartea_runtime.$$curry(tagger, [value])), $s1))($s2)($s3));
    }
  }
};
export { batch, map, none };
