// Compiled by dartea, an independent compiler. Not affiliated with or
// endorsed by the Elm project.
// Contains material derived from elm/html,
// Copyright (c) 2014-present Evan Czaplicki, under the BSD 3-Clause License.
// dartea's LICENSE carries the full text.
import * as VirtualDom from "./VirtualDom.mjs";
const on = (eta1, eta2) => VirtualDom.on(eta1, eta2);
const onClick = eta1$1 => on("click", eta1$1);
export { on, onClick };
