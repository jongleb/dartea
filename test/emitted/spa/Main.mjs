import * as Browser from "./Browser.mjs";
import * as Browser$Navigation from "./Browser.Navigation.mjs";
import * as Html from "./Html.mjs";
import * as Html$Attributes from "./Html.Attributes.mjs";
import * as Maybe from "./Maybe.mjs";
import * as Platform$Cmd from "./Platform.Cmd.mjs";
import * as Platform$Sub from "./Platform.Sub.mjs";
import * as $$String from "./String.mjs";
import * as Url from "./Url.mjs";
import * as Url$Parser from "./Url.Parser.mjs";
const Home = "Home";
const Post = _0 => ({ _0: _0 });
const Unknown = "Unknown";
const Clicked = _0 => ({ TAG: "Clicked", _0: _0 });
const Changed = _0 => ({ TAG: "Changed", _0: _0 });
const route = Url$Parser.oneOf({ hd: Url$Parser.map(Home, Url$Parser.top), tl: { hd: Url$Parser.map(Post, Url$Parser.$$lt$slash$gt(Url$Parser.s("post"), Url$Parser.$$int)), tl: 0 } });
const routeOf = url => Maybe.withDefault(Unknown, Url$Parser.parse(route, url));
const init = ($p0, url$1, key) => [{ key: key, route: routeOf(url$1) }, Platform$Cmd.none];
const update = (msg, model) => {
  switch (msg.TAG) {
    case "Clicked":
      switch (msg._0.TAG) {
        case "Internal":
          const url$2 = msg._0._0;
          return [model, Browser$Navigation.pushUrl(model.key, Url.toString(url$2))];
        case "External":
          const href = msg._0._0;
          return [model, Browser$Navigation.load(href)];
      }
    case "Changed":
      const url$3 = msg._0;
      return [{ ...model, route: routeOf(url$3) }, Platform$Cmd.none];
  }
};
const title = found => {
  if (found === "Home") {
    return "Home";
  } else {
    if (typeof found === "object") {
      const id = found._0;
      return "Post " + $$String.fromInt(id);
    } else {
      return "Not found";
    }
  }
};
const view = model$1 => ({ title: title(model$1.route), body: { hd: Html.h1(0, { hd: Html.text(title(model$1.route)), tl: 0 }), tl: { hd: Html.ul(0, { hd: Html.li(0, { hd: Html.a({ hd: Html$Attributes.href("/"), tl: 0 }, { hd: Html.text("home"), tl: 0 }), tl: 0 }), tl: { hd: Html.li(0, { hd: Html.a({ hd: Html$Attributes.href("/post/7"), tl: 0 }, { hd: Html.text("post 7"), tl: 0 }), tl: 0 }), tl: { hd: Html.li(0, { hd: Html.a({ hd: Html$Attributes.href("https://elm-lang.org"), tl: 0 }, { hd: Html.text("elsewhere"), tl: 0 }), tl: 0 }), tl: 0 } } }), tl: 0 } } });
const main = Browser.application({ init: init, view: view, update: update, subscriptions: $p0$1 => Platform$Sub.none, onUrlRequest: Clicked, onUrlChange: Changed });
const Model = ($a0, $a1) => ({ key: $a0, route: $a1 });
export { Changed, Clicked, Home, Model, Post, Unknown, main, routeOf };
