// Compiled by dartea, an independent compiler. Not affiliated with or
// endorsed by the Elm project.
// Contains material derived from elm/html,
// Copyright (c) 2014-present Evan Czaplicki, under the BSD 3-Clause License.
// dartea's LICENSE carries the full text.
import * as VirtualDom from "./VirtualDom.mjs";
const node = (eta1, eta2, eta3) => VirtualDom.keyedNode(eta1, eta2, eta3);
const ol = (eta1$1, eta2$1) => node("ol", eta1$1, eta2$1);
const ul = (eta1$2, eta2$2) => node("ul", eta1$2, eta2$2);
export { node, ol, ul };
