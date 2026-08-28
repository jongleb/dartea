// Compiled by dartea, an independent compiler. Not affiliated with or
// endorsed by the Elm project.
// Contains material derived from elm/virtual-dom,
// Copyright (c) 2016-present Evan Czaplicki, under the BSD 3-Clause License.
// dartea's LICENSE carries the full text.
import * as Dartea_browser from "./Dartea_browser.mjs";
const Node = "Node";
const Normal = _0 => ({ TAG: "Normal", _0: _0 });
const MayStopPropagation = _0 => ({ TAG: "MayStopPropagation", _0: _0 });
const MayPreventDefault = _0 => ({ TAG: "MayPreventDefault", _0: _0 });
const Custom = _0 => ({ TAG: "Custom", _0: _0 });
const Attribute = "Attribute";
const node = Dartea_browser.$$VirtualDom$node;
const keyedNode = Dartea_browser.$$VirtualDom$keyedNode;
const text = Dartea_browser.$$VirtualDom$text;
const attribute = Dartea_browser.$$VirtualDom$attribute;
const property = Dartea_browser.$$VirtualDom$property;
const style = Dartea_browser.$$VirtualDom$style;
const on = Dartea_browser.$$VirtualDom$on;
const map = Dartea_browser.$$VirtualDom$map;
const mapAttribute = Dartea_browser.$$VirtualDom$mapAttribute;
export { Custom, MayPreventDefault, MayStopPropagation, Normal, attribute, keyedNode, map, mapAttribute, node, on, property, style, text };
