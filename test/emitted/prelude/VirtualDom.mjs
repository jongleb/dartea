// Compiled by dartea, an independent compiler. Not affiliated with or
// endorsed by the Elm project.
// Contains material derived from elm/virtual-dom,
// Copyright (c) 2016-present Evan Czaplicki, under the BSD 3-Clause License.
// dartea's LICENSE carries the full text.
import * as Dartea_browser from "./Dartea_browser.mjs";
import * as Basics from "./Basics.mjs";
import * as Json$Decode from "./Json.Decode.mjs";
import * as Json$Encode from "./Json.Encode.mjs";
import * as Result from "./Result.mjs";
import * as $$String from "./String.mjs";
const Normal = _0 => ({ TAG: "Normal", _0: _0 });
const MayStopPropagation = _0 => ({ TAG: "MayStopPropagation", _0: _0 });
const MayPreventDefault = _0 => ({ TAG: "MayPreventDefault", _0: _0 });
const Custom = _0 => ({ TAG: "Custom", _0: _0 });
const noScript = tag => {
  if (tag === "script") {
    return "p";
  } else {
    return tag;
  }
};
const noOnOrFormAction = key => {
  const lowered = $$String.toLower(key);
  if ($$String.startsWith("on", lowered) || (lowered === "formaction")) {
    return "data-" + key;
  } else {
    return key;
  }
};
const noInnerHtmlOrFormAction = key$1 => {
  if ((key$1 === "innerHTML") || (key$1 === "formAction")) {
    return "data-" + key$1;
  } else {
    return key$1;
  }
};
const notWhitespace = letter => Basics.not($$String.isEmpty($$String.trim($$String.fromChar(letter))));
const dangerousUri = value => {
  const squeezed = $$String.toLower($$String.filter(notWhitespace, value));
  const trimmed = $$String.toLower($$String.trimLeft(value));
  return $$String.startsWith("javascript:", squeezed) || $$String.startsWith("data:text/html", trimmed);
};
const noJavaScriptOrHtmlUri = value$1 => {
  if (dangerousUri(value$1)) {
    return "";
  } else {
    return value$1;
  }
};
const noJavaScriptOrHtmlJson = value$2 => {
  const $s1 = Json$Decode.decodeValue(Json$Decode.string, value$2);
  switch ($s1.TAG) {
    case "Ok":
      const written = $s1._0;
      if (dangerousUri(written)) {
        return Json$Encode.string("");
      } else {
        return value$2;
      }
    case "Err":
      return value$2;
  }
};
const node = (tag$1, eta1, eta2) => Dartea_browser.$$VirtualDom$node(noScript(tag$1), eta1, eta2);
const nodeNS = (namespace, tag$2, eta1$1, eta2$1) => Dartea_browser.$$VirtualDom$nodeNS(namespace, noScript(tag$2), eta1$1, eta2$1);
const keyedNode = (tag$3, eta1$2, eta2$2) => Dartea_browser.$$VirtualDom$keyedNode(noScript(tag$3), eta1$2, eta2$2);
const keyedNodeNS = (namespace$1, tag$4, eta1$3, eta2$3) => Dartea_browser.$$VirtualDom$keyedNodeNS(namespace$1, noScript(tag$4), eta1$3, eta2$3);
const text = Dartea_browser.$$VirtualDom$text;
const attribute = (key$2, value$3) => Dartea_browser.$$VirtualDom$attribute(noOnOrFormAction(key$2), noJavaScriptOrHtmlUri(value$3));
const attributeNS = (namespace$2, key$3, value$4) => Dartea_browser.$$VirtualDom$attributeNS(namespace$2, noOnOrFormAction(key$3), noJavaScriptOrHtmlUri(value$4));
const property = (key$4, value$5) => Dartea_browser.$$VirtualDom$property(noInnerHtmlOrFormAction(key$4), noJavaScriptOrHtmlJson(value$5));
const style = Dartea_browser.$$VirtualDom$style;
const on = Dartea_browser.$$VirtualDom$on;
const map = Dartea_browser.$$VirtualDom$map;
const mapAttribute = Dartea_browser.$$VirtualDom$mapAttribute;
const lazy = Dartea_browser.$$VirtualDom$lazy;
const lazy2 = Dartea_browser.$$VirtualDom$lazy2;
const lazy3 = Dartea_browser.$$VirtualDom$lazy3;
const lazy4 = Dartea_browser.$$VirtualDom$lazy4;
const lazy5 = Dartea_browser.$$VirtualDom$lazy5;
const lazy6 = Dartea_browser.$$VirtualDom$lazy6;
const lazy7 = Dartea_browser.$$VirtualDom$lazy7;
const lazy8 = Dartea_browser.$$VirtualDom$lazy8;
export { Custom, MayPreventDefault, MayStopPropagation, Normal, attribute, attributeNS, keyedNode, keyedNodeNS, lazy, lazy2, lazy3, lazy4, lazy5, lazy6, lazy7, lazy8, map, mapAttribute, node, nodeNS, on, property, style, text };
