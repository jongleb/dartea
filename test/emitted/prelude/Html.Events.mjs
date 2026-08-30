// Compiled by dartea, an independent compiler. Not affiliated with or
// endorsed by the Elm project.
import * as Json$Decode from "./Json.Decode.mjs";
import * as VirtualDom from "./VirtualDom.mjs";
const on = (event, decoder) => VirtualDom.on(event, VirtualDom.Normal(decoder));
const stopPropagationOn = (event$1, decoder$1) => VirtualDom.on(event$1, VirtualDom.MayStopPropagation(decoder$1));
const preventDefaultOn = (event$2, decoder$2) => VirtualDom.on(event$2, VirtualDom.MayPreventDefault(decoder$2));
const custom = (event$3, decoder$3) => VirtualDom.on(event$3, VirtualDom.Custom(decoder$3));
const onClick = eta1 => VirtualDom.on("click", VirtualDom.Normal(Json$Decode.succeed(eta1)));
const onDoubleClick = eta1$1 => VirtualDom.on("dblclick", VirtualDom.Normal(Json$Decode.succeed(eta1$1)));
const onMouseDown = eta1$2 => VirtualDom.on("mousedown", VirtualDom.Normal(Json$Decode.succeed(eta1$2)));
const onMouseUp = eta1$3 => VirtualDom.on("mouseup", VirtualDom.Normal(Json$Decode.succeed(eta1$3)));
const onMouseEnter = eta1$4 => VirtualDom.on("mouseenter", VirtualDom.Normal(Json$Decode.succeed(eta1$4)));
const onMouseLeave = eta1$5 => VirtualDom.on("mouseleave", VirtualDom.Normal(Json$Decode.succeed(eta1$5)));
const onMouseOver = eta1$6 => VirtualDom.on("mouseover", VirtualDom.Normal(Json$Decode.succeed(eta1$6)));
const onMouseOut = eta1$7 => VirtualDom.on("mouseout", VirtualDom.Normal(Json$Decode.succeed(eta1$7)));
const onBlur = eta1$8 => VirtualDom.on("blur", VirtualDom.Normal(Json$Decode.succeed(eta1$8)));
const onFocus = eta1$9 => VirtualDom.on("focus", VirtualDom.Normal(Json$Decode.succeed(eta1$9)));
const targetValue = Json$Decode.at({ hd: "target", tl: { hd: "value", tl: 0 } }, Json$Decode.string);
const targetChecked = Json$Decode.at({ hd: "target", tl: { hd: "checked", tl: 0 } }, Json$Decode.bool);
const keyCode = Json$Decode.field("keyCode", Json$Decode.$$int);
const alwaysTrue = given => [given, true];
const onInput = tagger => VirtualDom.on("input", VirtualDom.MayStopPropagation(Json$Decode.map(alwaysTrue, Json$Decode.map(tagger, targetValue))));
const onCheck = tagger$1 => VirtualDom.on("change", VirtualDom.Normal(Json$Decode.map(tagger$1, targetChecked)));
const onSubmit = given$1 => VirtualDom.on("submit", VirtualDom.MayPreventDefault(Json$Decode.map(alwaysTrue, Json$Decode.succeed(given$1))));
export { custom, keyCode, on, onBlur, onCheck, onClick, onDoubleClick, onFocus, onInput, onMouseDown, onMouseEnter, onMouseLeave, onMouseOut, onMouseOver, onMouseUp, onSubmit, preventDefaultOn, stopPropagationOn, targetChecked, targetValue };
