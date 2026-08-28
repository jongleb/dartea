// Compiled by dartea, an independent compiler. Not affiliated with or
// endorsed by the Elm project.
// Contains material derived from elm/html,
// Copyright (c) 2014-present Evan Czaplicki, under the BSD 3-Clause License.
// dartea's LICENSE carries the full text.
import * as Json$Decode from "./Json.Decode.mjs";
import * as VirtualDom from "./VirtualDom.mjs";
const on = (event, decoder) => VirtualDom.on(event, VirtualDom.Normal(decoder));
const stopPropagationOn = (event$1, decoder$1) => VirtualDom.on(event$1, VirtualDom.MayStopPropagation(decoder$1));
const preventDefaultOn = (event$2, decoder$2) => VirtualDom.on(event$2, VirtualDom.MayPreventDefault(decoder$2));
const custom = (event$3, decoder$3) => VirtualDom.on(event$3, VirtualDom.Custom(decoder$3));
const onClick = eta1 => {
  const decoder$4 = Json$Decode.succeed(eta1);
  return VirtualDom.on("click", VirtualDom.Normal(decoder$4));
};
const onDoubleClick = eta1$1 => {
  const decoder$5 = Json$Decode.succeed(eta1$1);
  return VirtualDom.on("dblclick", VirtualDom.Normal(decoder$5));
};
const onMouseDown = eta1$2 => {
  const decoder$6 = Json$Decode.succeed(eta1$2);
  return VirtualDom.on("mousedown", VirtualDom.Normal(decoder$6));
};
const onMouseUp = eta1$3 => {
  const decoder$7 = Json$Decode.succeed(eta1$3);
  return VirtualDom.on("mouseup", VirtualDom.Normal(decoder$7));
};
const onMouseEnter = eta1$4 => {
  const decoder$8 = Json$Decode.succeed(eta1$4);
  return VirtualDom.on("mouseenter", VirtualDom.Normal(decoder$8));
};
const onMouseLeave = eta1$5 => {
  const decoder$9 = Json$Decode.succeed(eta1$5);
  return VirtualDom.on("mouseleave", VirtualDom.Normal(decoder$9));
};
const onMouseOver = eta1$6 => {
  const decoder$10 = Json$Decode.succeed(eta1$6);
  return VirtualDom.on("mouseover", VirtualDom.Normal(decoder$10));
};
const onMouseOut = eta1$7 => {
  const decoder$11 = Json$Decode.succeed(eta1$7);
  return VirtualDom.on("mouseout", VirtualDom.Normal(decoder$11));
};
const onBlur = eta1$8 => {
  const decoder$12 = Json$Decode.succeed(eta1$8);
  return VirtualDom.on("blur", VirtualDom.Normal(decoder$12));
};
const onFocus = eta1$9 => {
  const decoder$13 = Json$Decode.succeed(eta1$9);
  return VirtualDom.on("focus", VirtualDom.Normal(decoder$13));
};
const targetValue = Json$Decode.at({ hd: "target", tl: { hd: "value", tl: 0 } }, Json$Decode.string);
const targetChecked = Json$Decode.at({ hd: "target", tl: { hd: "checked", tl: 0 } }, Json$Decode.bool);
const keyCode = Json$Decode.field("keyCode", Json$Decode.$$int);
const alwaysTrue = given => [given, true];
const onInput = tagger => {
  const decoder$14 = Json$Decode.map(alwaysTrue, Json$Decode.map(tagger, targetValue));
  return VirtualDom.on("input", VirtualDom.MayStopPropagation(decoder$14));
};
const onCheck = tagger$1 => {
  const decoder$15 = Json$Decode.map(tagger$1, targetChecked);
  return VirtualDom.on("change", VirtualDom.Normal(decoder$15));
};
const onSubmit = given$1 => {
  const decoder$16 = Json$Decode.map(alwaysTrue, Json$Decode.succeed(given$1));
  return VirtualDom.on("submit", VirtualDom.MayPreventDefault(decoder$16));
};
export { custom, keyCode, on, onBlur, onCheck, onClick, onDoubleClick, onFocus, onInput, onMouseDown, onMouseEnter, onMouseLeave, onMouseOut, onMouseOver, onMouseUp, onSubmit, preventDefaultOn, stopPropagationOn, targetChecked, targetValue };
