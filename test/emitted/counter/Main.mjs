import * as Browser from "./Browser.mjs";
import * as Json$Decode from "./Json.Decode.mjs";
import * as $$String from "./String.mjs";
import * as VirtualDom from "./VirtualDom.mjs";
const Bumped = "Bumped";
const Reset = "Reset";
const $$form0 = { tag: "div", attributes: [{ key: "className", value: "counter", way: "property" }], children: [{ tag: "button", attributes: [], children: [{ text: "+" }] }, { hole: 1 }, { tag: "button", attributes: [], children: [{ text: "reset" }] }], holes: [{ path: [0], kind: "event" }, { path: [1], kind: "text" }, { path: [2], kind: "event" }] };
const update = (msg, model) => {
  switch (msg) {
    case "Bumped":
      return model + 1;
    case "Reset":
      return 0;
  }
};
const view = model$1 => ({ TAG: "block", form: $$form0, values: [VirtualDom.on("click", VirtualDom.Normal(Json$Decode.succeed(Bumped))), " " + ($$String.fromInt(model$1) + " "), VirtualDom.on("click", VirtualDom.Normal(Json$Decode.succeed(Reset)))] });
const main = Browser.sandbox({ init: 0, update: update, view: view });
export { main };
