import * as Browser from "./Browser.mjs";
import * as Html from "./Html.mjs";
import * as Html$Attributes from "./Html.Attributes.mjs";
import * as Html$Events from "./Html.Events.mjs";
import * as $$String from "./String.mjs";
const Bumped = "Bumped";
const Reset = "Reset";
const update = (msg, model) => {
  switch (msg) {
    case "Bumped":
      return model + 1;
    case "Reset":
      return 0;
  }
};
const view = model$1 => Html.div({ hd: Html$Attributes.$$class("counter"), tl: 0 }, { hd: Html.button({ hd: Html$Events.onClick(Bumped), tl: 0 }, { hd: Html.text("+"), tl: 0 }), tl: { hd: Html.text(" " + ($$String.fromInt(model$1) + " ")), tl: { hd: Html.button({ hd: Html$Events.onClick(Reset), tl: 0 }, { hd: Html.text("reset"), tl: 0 }), tl: 0 } } });
const main = Browser.sandbox({ init: 0, update: update, view: view });
export { main };
