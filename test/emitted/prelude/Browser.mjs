// Compiled by dartea, an independent compiler. Not affiliated with or
// endorsed by the Elm project.
// Contains material derived from elm/browser,
// Copyright 2017-present Evan Czaplicki, under the BSD 3-Clause License.
// dartea's LICENSE carries the full text.
import * as Dartea_browser from "./Dartea_browser.mjs";
import * as Url from "./Url.mjs";
const Internal = _0 => ({ TAG: "Internal", _0: _0 });
const External = _0 => ({ TAG: "External", _0: _0 });
const sandbox = Dartea_browser.$$Browser$sandbox;
const element = Dartea_browser.$$Browser$element;
const document = Dartea_browser.$$Browser$document;
const applicationWith = Dartea_browser.$$Browser$application;
const application = impl => applicationWith(Url.fromString, impl);
const Document = ($a0, $a1) => ({ title: $a0, body: $a1 });
export { Document, External, Internal, application, document, element, sandbox };
