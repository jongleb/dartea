// Compiled by dartea, an independent compiler. Not affiliated with or
// endorsed by the Elm project.
// Contains material derived from elm/html,
// Copyright (c) 2014-present Evan Czaplicki, under the BSD 3-Clause License.
// dartea's LICENSE carries the full text.
import * as VirtualDom from "./VirtualDom.mjs";
const node = (eta1, eta2, eta3) => VirtualDom.node(eta1, eta2, eta3);
const text = eta1$1 => VirtualDom.text(eta1$1);
const a = (eta1$2, eta2$1) => node("a", eta1$2, eta2$1);
const button = (eta1$3, eta2$2) => node("button", eta1$3, eta2$2);
const div = (eta1$4, eta2$3) => node("div", eta1$4, eta2$3);
const h1 = (eta1$5, eta2$4) => node("h1", eta1$5, eta2$4);
const input = (eta1$6, eta2$5) => node("input", eta1$6, eta2$5);
const li = (eta1$7, eta2$6) => node("li", eta1$7, eta2$6);
const p = (eta1$8, eta2$7) => node("p", eta1$8, eta2$7);
const span = (eta1$9, eta2$8) => node("span", eta1$9, eta2$8);
const ul = (eta1$10, eta2$9) => node("ul", eta1$10, eta2$9);
export { a, button, div, h1, input, li, node, p, span, text, ul };
