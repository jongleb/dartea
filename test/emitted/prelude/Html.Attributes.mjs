// Compiled by dartea, an independent compiler. Not affiliated with or
// endorsed by the Elm project.
// Contains material derived from elm/html,
// Copyright (c) 2014-present Evan Czaplicki, under the BSD 3-Clause License.
// dartea's LICENSE carries the full text.
import * as Json$Encode from "./Json.Encode.mjs";
import * as List from "./List.mjs";
import * as $$String from "./String.mjs";
import * as Tuple from "./Tuple.mjs";
import * as VirtualDom from "./VirtualDom.mjs";
const $$class = eta1 => VirtualDom.property("className", Json$Encode.string(eta1));
const classList = pairs => {
  const eta1$1 = $$String.join(" ", List.map(Tuple.first, List.filter(Tuple.second, pairs)));
  return VirtualDom.property("className", Json$Encode.string(eta1$1));
};
const id = eta1$2 => VirtualDom.property("id", Json$Encode.string(eta1$2));
const title = eta1$3 => VirtualDom.property("title", Json$Encode.string(eta1$3));
const hidden = eta1$4 => VirtualDom.property("hidden", Json$Encode.bool(eta1$4));
const type_ = eta1$5 => VirtualDom.property("type", Json$Encode.string(eta1$5));
const value = eta1$6 => VirtualDom.property("value", Json$Encode.string(eta1$6));
const checked = eta1$7 => VirtualDom.property("checked", Json$Encode.bool(eta1$7));
const placeholder = eta1$8 => VirtualDom.property("placeholder", Json$Encode.string(eta1$8));
const autofocus = eta1$9 => VirtualDom.property("autofocus", Json$Encode.bool(eta1$9));
const name = eta1$10 => VirtualDom.property("name", Json$Encode.string(eta1$10));
const $$for = eta1$11 => VirtualDom.property("htmlFor", Json$Encode.string(eta1$11));
const href = eta1$12 => VirtualDom.property("href", Json$Encode.string(eta1$12));
const src = eta1$13 => VirtualDom.property("src", Json$Encode.string(eta1$13));
const style = (eta1$14, eta2) => VirtualDom.style(eta1$14, eta2);
const map = (eta1$15, eta2$1) => VirtualDom.mapAttribute(eta1$15, eta2$1);
export { autofocus, checked, $$class, classList, $$for, hidden, href, id, map, name, placeholder, src, style, title, type_, value };
