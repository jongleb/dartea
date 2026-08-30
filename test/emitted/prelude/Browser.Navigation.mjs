// Compiled by dartea, an independent compiler. Not affiliated with or
// endorsed by the Elm project.
// Contains material derived from elm/browser,
// Copyright 2017-present Evan Czaplicki, under the BSD 3-Clause License.
// dartea's LICENSE carries the full text.
import * as Dartea_browser from "./Dartea_browser.mjs";
import * as Basics from "./Basics.mjs";
const pushUrl = Dartea_browser.$$Browser$pushUrl;
const replaceUrl = Dartea_browser.$$Browser$replaceUrl;
const go = Dartea_browser.$$Browser$go;
const back = (key, n) => go(key, Basics.negate(n));
const forward = (eta1, eta2) => go(eta1, eta2);
const load = Dartea_browser.$$Browser$load;
const reloadWith = Dartea_browser.$$Browser$reload;
const reload = reloadWith(false);
const reloadAndSkipCache = reloadWith(true);
export { back, forward, load, pushUrl, reload, reloadAndSkipCache, replaceUrl };
