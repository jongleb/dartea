// Compiled by dartea, an independent compiler. Not affiliated with or
// endorsed by the Elm project.
// Contains material derived from elm/browser,
// Copyright 2017-present Evan Czaplicki, under the BSD 3-Clause License.
// dartea's LICENSE carries the full text.
import * as Dartea_runtime from "./Dartea_runtime.mjs";
import * as Dartea_browser from "./Dartea_browser.mjs";
import * as Json$Decode from "./Json.Decode.mjs";
import * as Time from "./Time.mjs";
const Visible = "Visible";
const Hidden = "Hidden";
const Document = "Document";
const Window = "Window";
const onAnimationFrame = toMsg => Dartea_browser.$$Browser$onAnimationFrame(millis => Dartea_runtime.$$curry(toMsg, [Time.millisToPosix(millis)]));
const onAnimationFrameDelta = Dartea_browser.$$Browser$onAnimationFrameDelta;
const listen = Dartea_browser.$$Browser$on;
const onKeyPress = eta1 => {
  let $s1;
  if (Document === "Document") {
    $s1 = "document";
  } else {
    $s1 = "window";
  }
  return listen($s1, "keypress", eta1);
};
const onKeyDown = eta1$1 => {
  let $s2;
  if (Document === "Document") {
    $s2 = "document";
  } else {
    $s2 = "window";
  }
  return listen($s2, "keydown", eta1$1);
};
const onKeyUp = eta1$2 => {
  let $s3;
  if (Document === "Document") {
    $s3 = "document";
  } else {
    $s3 = "window";
  }
  return listen($s3, "keyup", eta1$2);
};
const onClick = eta1$3 => {
  let $s4;
  if (Document === "Document") {
    $s4 = "document";
  } else {
    $s4 = "window";
  }
  return listen($s4, "click", eta1$3);
};
const onMouseMove = eta1$4 => {
  let $s5;
  if (Document === "Document") {
    $s5 = "document";
  } else {
    $s5 = "window";
  }
  return listen($s5, "mousemove", eta1$4);
};
const onMouseDown = eta1$5 => {
  let $s6;
  if (Document === "Document") {
    $s6 = "document";
  } else {
    $s6 = "window";
  }
  return listen($s6, "mousedown", eta1$5);
};
const onMouseUp = eta1$6 => {
  let $s7;
  if (Document === "Document") {
    $s7 = "document";
  } else {
    $s7 = "window";
  }
  return listen($s7, "mouseup", eta1$6);
};
const onResize = func => {
  const decoder = Json$Decode.field("target", Json$Decode.map2(func, Json$Decode.field("innerWidth", Json$Decode.$$int), Json$Decode.field("innerHeight", Json$Decode.$$int)));
  let $s8;
  if (Window === "Document") {
    $s8 = "document";
  } else {
    $s8 = "window";
  }
  return listen($s8, "resize", decoder);
};
const withHidden = (func$1, isHidden) => Dartea_runtime.$$curry(func$1, [isHidden ? Hidden : Visible]);
const onVisibilityChange = func$2 => {
  const decoder$1 = Json$Decode.map($s9 => withHidden(func$2, $s9), Json$Decode.field("target", Json$Decode.field("hidden", Json$Decode.bool)));
  let $s10;
  if (Document === "Document") {
    $s10 = "document";
  } else {
    $s10 = "window";
  }
  return listen($s10, "visibilitychange", decoder$1);
};
export { Hidden, Visible, onAnimationFrame, onAnimationFrameDelta, onClick, onKeyDown, onKeyPress, onKeyUp, onMouseDown, onMouseMove, onMouseUp, onResize, onVisibilityChange };
