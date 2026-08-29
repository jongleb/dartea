// Compiled by dartea, an independent compiler. Not affiliated with or
// endorsed by the Elm project.
// Contains material derived from elm/core,
// Copyright 2014-present Evan Czaplicki, under the BSD 3-Clause License.
// dartea's LICENSE carries the full text.
import * as Dartea_runtime from "./Dartea_runtime.mjs";
import * as List from "./List.mjs";
const None = "None";
const Batch = _0 => ({ TAG: "Batch", _0: _0 });
const Perform = _0 => ({ TAG: "Perform", _0: _0 });
const none = None;
const batch = eta1 => Batch(eta1);
const map = (tagger, command) => {
  if (command === "None") {
    return None;
  } else {
    if (command.TAG === "Batch") {
      const commands = command._0;
      return Batch(List.map($s1 => map(tagger, $s1), commands));
    } else {
      const run = command._0;
      return Perform(dispatch => run(value => dispatch(Dartea_runtime.$$curry(tagger, [value]))));
    }
  }
};
export { batch, map, none };
