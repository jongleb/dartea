// Compiled by dartea, an independent compiler. Not affiliated with or
// endorsed by the Elm project.
// Contains material derived from elm/html,
// Copyright (c) 2014-present Evan Czaplicki, under the BSD 3-Clause License.
// dartea's LICENSE carries the full text.
import * as VirtualDom from "./VirtualDom.mjs";
const node = (eta1, eta2, eta3) => VirtualDom.node(eta1, eta2, eta3);
const map = (eta1$1, eta2$1) => VirtualDom.map(eta1$1, eta2$1);
const text = eta1$2 => VirtualDom.text(eta1$2);
const a = (eta1$3, eta2$2) => node("a", eta1$3, eta2$2);
const button = (eta1$4, eta2$3) => node("button", eta1$4, eta2$3);
const div = (eta1$5, eta2$4) => node("div", eta1$5, eta2$4);
const footer = (eta1$6, eta2$5) => node("footer", eta1$6, eta2$5);
const form = (eta1$7, eta2$6) => node("form", eta1$7, eta2$6);
const header = (eta1$8, eta2$7) => node("header", eta1$8, eta2$7);
const label = (eta1$9, eta2$8) => node("label", eta1$9, eta2$8);
const section = (eta1$10, eta2$9) => node("section", eta1$10, eta2$9);
const strong = (eta1$11, eta2$10) => node("strong", eta1$11, eta2$10);
const h1 = (eta1$12, eta2$11) => node("h1", eta1$12, eta2$11);
const input = (eta1$13, eta2$12) => node("input", eta1$13, eta2$12);
const li = (eta1$14, eta2$13) => node("li", eta1$14, eta2$13);
const p = (eta1$15, eta2$14) => node("p", eta1$15, eta2$14);
const span = (eta1$16, eta2$15) => node("span", eta1$16, eta2$15);
const ul = (eta1$17, eta2$16) => node("ul", eta1$17, eta2$16);
export { a, button, div, footer, form, h1, header, input, label, li, map, node, p, section, span, strong, text, ul };
