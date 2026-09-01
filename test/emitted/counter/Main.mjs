import * as Browser from "./Browser.mjs";
import * as $$String from "./String.mjs";
const Bumped = "Bumped";
const Reset = "Reset";
const $$form0 = { tag: "div", attributes: [{ key: "className", value: "counter", way: "property" }], children: [{ tag: "button", attributes: [], children: [{ text: "+" }] }, { hole: 1 }, { tag: "button", attributes: [], children: [{ text: "reset" }] }], holes: [{ path: [0], find: $$e => $$e.firstChild, kind: "event", event: "click", plain: true }, { path: [1], find: $$e => $$e.firstChild.nextSibling, kind: "text" }, { path: [2], find: $$e => $$e.firstChild.nextSibling.nextSibling, kind: "event", event: "click", plain: true }], guards: 1 };
const $$r0 = ($$b, $$put, $$v) => {
  const model$1 = $$v.a0;
  $$put($$b, 0, Bumped);
  if ($$b.deps[0] !== model$1) {
    $$b.deps[0] = model$1;
    $$put($$b, 1, " " + ($$String.fromInt(model$1) + " "));
  }
  $$put($$b, 2, Reset);
};
const update = (msg, model) => {
  switch (msg) {
    case "Bumped":
      return model + 1;
    case "Reset":
      return 0;
  }
};
const view = model$1 => ({ TAG: "block", form: $$form0, refresh: $$r0, a0: model$1 });
const main = Browser.sandbox({ init: 0, update: update, view: view });
export { main };
