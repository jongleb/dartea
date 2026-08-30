import * as Browser from "./Browser.mjs";
import * as Http from "./Http.mjs";
import * as Json$Decode from "./Json.Decode.mjs";
import * as Json$Encode from "./Json.Encode.mjs";
import * as Platform$Cmd from "./Platform.Cmd.mjs";
import * as Platform$Sub from "./Platform.Sub.mjs";
import * as Result from "./Result.mjs";
import * as $$String from "./String.mjs";
const Fetch = "Fetch";
const Got = _0 => ({ TAG: "Got", _0: _0 });
const Posted = _0 => ({ TAG: "Posted", _0: _0 });
const $$form0 = { tag: "div", attributes: [], children: [{ tag: "p", attributes: [{ key: "id", value: "greeting", way: "property" }], children: [{ hole: 0 }] }, { tag: "p", attributes: [{ key: "id", value: "answer", way: "property" }], children: [{ hole: 1 }] }, { tag: "p", attributes: [{ key: "id", value: "failure", way: "property" }], children: [{ hole: 2 }] }, { tag: "button", attributes: [], children: [{ text: "double" }] }], holes: [{ path: [0, 0], kind: "text" }, { path: [1, 0], kind: "text" }, { path: [2, 0], kind: "text" }, { path: [3], kind: "event", event: "click", plain: true }] };
const $$r0 = ($$b, $$put, $$a) => {
  const model$1 = $$a[0];
  $$put($$b, 0, model$1.greeting);
  $$put($$b, 1, model$1.answer);
  $$put($$b, 2, model$1.failure);
  $$put($$b, 3, Fetch);
};
const init = $p0 => [{ greeting: "", answer: "", failure: "" }, Http.get({ url: "/hello", expect: Http.expectString(Got) })];
const describe = problem => {
  if (problem.TAG === "BadUrl") {
    const url = problem._0;
    return "bad url " + url;
  } else {
    if (problem === "Timeout") {
      return "timeout";
    } else {
      if (problem === "NetworkError") {
        return "network";
      } else {
        if (problem.TAG === "BadStatus") {
          const code = problem._0;
          return "status " + $$String.fromInt(code);
        } else {
          const reason = problem._0;
          return "body " + reason;
        }
      }
    }
  }
};
const update = (msg, model) => {
  if (msg === "Fetch") {
    return [model, Http.post({ url: "/double", body: Http.jsonBody(Json$Encode.object({ hd: ["n", Json$Encode.$$int(21)], tl: 0 })), expect: Http.expectJson(Posted, Json$Decode.field("doubled", Json$Decode.$$int)) })];
  } else {
    if (msg.TAG === "Got") {
      switch (msg._0.TAG) {
        case "Ok":
          const greeting = msg._0._0;
          return [{ ...model, greeting: greeting }, Platform$Cmd.none];
        case "Err":
          const problem$2 = msg._0._0;
          return [{ ...model, failure: describe(problem$2) }, Platform$Cmd.none];
      }
    } else {
      switch (msg._0.TAG) {
        case "Ok":
          const doubled = msg._0._0;
          return [{ ...model, answer: $$String.fromInt(doubled) }, Platform$Cmd.none];
        case "Err":
          const problem$1 = msg._0._0;
          return [{ ...model, failure: describe(problem$1) }, Platform$Cmd.none];
      }
    }
  }
};
const view = model$1 => ({ TAG: "block", form: $$form0, refresh: $$r0, args: [model$1] });
const main = Browser.element({ init: init, view: view, update: update, subscriptions: $p0$1 => Platform$Sub.none });
const Model = ($a0, $a1, $a2) => ({ greeting: $a0, answer: $a1, failure: $a2 });
export { Fetch, Got, Model, Posted, main };
