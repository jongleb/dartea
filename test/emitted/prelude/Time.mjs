// Compiled by dartea, an independent compiler. Not affiliated with or
// endorsed by the Elm project.
// Contains material derived from elm/time,
// Copyright 2018-present Evan Czaplicki, under the BSD 3-Clause License.
// dartea's LICENSE carries the full text.
import * as Dartea_browser from "./Dartea_browser.mjs";
const Posix = _0 => ({ _0: _0 });
const millisToPosix = eta1 => Posix(eta1);
const posixToMillis = stamp => {
  const millis = stamp._0;
  return millis;
};
const every = Dartea_browser.$$Time$every;
export { every, millisToPosix, posixToMillis };
