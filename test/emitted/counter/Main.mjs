import * as Browser from "./Browser.mjs";
import * as $$String from "./String.mjs";
const Bumped = "Bumped";
const Reset = "Reset";
const $$form0 = { tag: "div", attributes: [{ key: "className", value: "counter", way: "property" }], children: [{ tag: "button", attributes: [], children: [{ text: "+" }] }, { hole: 1 }, { tag: "button", attributes: [], children: [{ text: "reset" }] }], holes: [{ path: [0], kind: "event", event: "click", plain: true }, { path: [1], kind: "text" }, { path: [2], kind: "event", event: "click", plain: true }] };
const update = (msg, model) => {
  switch (msg) {
    case "Bumped":
      return model + 1;
    case "Reset":
      return 0;
  }
};
const view = model$1 => ({ TAG: "block", form: $$form0, refresh: ($$b, $$put) => {
  $$put($$b, 0, Bumped);
  if ($$b.deps[1] !== model$1) {
    $$b.deps[1] = model$1;
    $$put($$b, 1, " " + ($$String.fromInt(model$1) + " "));
  }
  $$put($$b, 2, Reset);
} });
const main = Browser.sandbox({ init: 0, update: update, view: view });
export { main };
