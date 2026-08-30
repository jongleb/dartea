import * as Browser from "./Browser.mjs";
import * as Browser$Navigation from "./Browser.Navigation.mjs";
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
const $$form0 = { tag: "h1", attributes: [], children: [{ hole: 0 }], holes: [{ path: [0], kind: "text" }] };
const $$form1 = { tag: "ul", attributes: [], children: [{ tag: "li", attributes: [], children: [{ tag: "a", attributes: [{ key: "href", value: "/", way: "property" }], children: [{ text: "home" }] }] }, { tag: "li", attributes: [], children: [{ tag: "a", attributes: [{ key: "href", value: "/post/7", way: "property" }], children: [{ text: "post 7" }] }] }, { tag: "li", attributes: [], children: [{ tag: "a", attributes: [{ key: "href", value: "https://elm-lang.org", way: "property" }], children: [{ text: "elsewhere" }] }] }], holes: [] };
const $$r0 = ($$b, $$put, $$a) => {
  const model$1 = $$a[0];
  if ($$b.deps[0] !== model$1.route) {
    $$b.deps[0] = model$1.route;
    $$put($$b, 0, title(model$1.route));
  }
};
const $$r1 = ($$b, $$put, $$a) => {

};
const route = Url$Parser.oneOf({ hd: Url$Parser.map(Home, Url$Parser.top), tl: { hd: Url$Parser.map(Post, Url$Parser.$$lt$slash$gt(Url$Parser.s("post"), Url$Parser.$$int)), tl: 0 } });
const routeOf = url => Maybe.withDefault(Unknown, Url$Parser.parse(route, url));
const init = ($p0, url$1, key) => [{ key: key, route: Maybe.withDefault(Unknown, Url$Parser.parse(route, url$1)) }, Platform$Cmd.none];
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
      return [{ ...model, route: Maybe.withDefault(Unknown, Url$Parser.parse(route, url$3)) }, Platform$Cmd.none];
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
const view = model$1 => ({ title: title(model$1.route), body: { hd: { TAG: "block", form: $$form0, refresh: $$r0, args: [model$1] }, tl: { hd: { TAG: "block", form: $$form1, refresh: $$r1, args: [] }, tl: 0 } } });
const main = Browser.application({ init: init, view: view, update: update, subscriptions: $p0$1 => Platform$Sub.none, onUrlRequest: Clicked, onUrlChange: Changed });
const Model = ($a0, $a1) => ({ key: $a0, route: $a1 });
export { Changed, Clicked, Home, Model, Post, Unknown, main, routeOf };
