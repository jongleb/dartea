import * as Dartea_browser from "./Dartea_browser.mjs";
import * as Basics from "./Basics.mjs";
import * as Browser from "./Browser.mjs";
import * as Browser$Dom from "./Browser.Dom.mjs";
import * as Html$Attributes from "./Html.Attributes.mjs";
import * as Html$Events from "./Html.Events.mjs";
import * as Html$Keyed from "./Html.Keyed.mjs";
import * as Html$Lazy from "./Html.Lazy.mjs";
import * as Json$Decode from "./Json.Decode.mjs";
import * as Json$Encode from "./Json.Encode.mjs";
import * as List from "./List.mjs";
import * as Maybe from "./Maybe.mjs";
import * as Platform$Cmd from "./Platform.Cmd.mjs";
import * as Platform$Sub from "./Platform.Sub.mjs";
import * as $$String from "./String.mjs";
import * as Task from "./Task.mjs";
import * as VirtualDom from "./VirtualDom.mjs";
const NoOp = "NoOp";
const UpdateField = _0 => ({ TAG: "UpdateField", _0: _0 });
const EditingEntry = (_0, _1) => ({ TAG: "EditingEntry", _0: _0, _1: _1 });
const UpdateEntry = (_0, _1) => ({ TAG: "UpdateEntry", _0: _0, _1: _1 });
const Add = "Add";
const Delete = _0 => ({ TAG: "Delete", _0: _0 });
const DeleteComplete = "DeleteComplete";
const Check = (_0, _1) => ({ TAG: "Check", _0: _0, _1: _1 });
const CheckAll = _0 => ({ TAG: "CheckAll", _0: _0 });
const ChangeVisibility = _0 => ({ TAG: "ChangeVisibility", _0: _0 });
const $append$List = (xs, ys) => {
  if (xs === 0) {
    return ys;
  }
  const root = { hd: xs.hd, tl: ys };
  let last = root;
  let rest = xs.tl;
  while (rest !== 0) {
    const copied = { hd: rest.hd, tl: ys };
    last.tl = copied;
    last = copied;
    rest = rest.tl;
  }
  return root;
};
const $$form0 = { tag: "button", attributes: [{ key: "className", value: "clear-completed", way: "property" }], children: [{ hole: 2 }], holes: [{ path: [], kind: "attribute" }, { path: [], kind: "event" }, { path: [0], kind: "text" }] };
const $$form1 = { tag: "span", attributes: [{ key: "className", value: "todo-count", way: "property" }], children: [{ tag: "strong", attributes: [], children: [{ hole: 0 }] }, { hole: 1 }], holes: [{ path: [0, 0], kind: "text" }, { path: [1], kind: "text" }] };
const $$form2 = { tag: "li", attributes: [], children: [{ tag: "a", attributes: [], children: [{ hole: 3 }] }], holes: [{ path: [], kind: "event" }, { path: [0], kind: "attribute" }, { path: [0], kind: "attribute" }, { path: [0, 0], kind: "text" }] };
const $$form3 = { tag: "ul", attributes: [{ key: "className", value: "filters", way: "property" }], children: [{ hole: 0 }, { text: " " }, { hole: 1 }, { text: " " }, { hole: 2 }], holes: [{ path: [0], kind: "subtree" }, { path: [2], kind: "subtree" }, { path: [4], kind: "subtree" }] };
const $$form4 = { tag: "footer", attributes: [{ key: "className", value: "footer", way: "property" }], children: [{ hole: 1 }, { hole: 2 }, { hole: 3 }], holes: [{ path: [], kind: "attribute" }, { path: [0], kind: "subtree" }, { path: [1], kind: "subtree" }, { path: [2], kind: "subtree" }] };
const $$form5 = { tag: "li", attributes: [], children: [{ tag: "div", attributes: [{ key: "className", value: "view", way: "property" }], children: [{ tag: "input", attributes: [{ key: "className", value: "toggle", way: "property" }, { key: "type", value: "checkbox", way: "property" }], children: [] }, { tag: "label", attributes: [], children: [{ hole: 4 }] }, { tag: "button", attributes: [{ key: "className", value: "destroy", way: "property" }], children: [] }] }, { tag: "input", attributes: [{ key: "className", value: "edit", way: "property" }, { key: "name", value: "title", way: "property" }], children: [] }], holes: [{ path: [], kind: "attribute" }, { path: [0, 0], kind: "attribute" }, { path: [0, 0], kind: "event" }, { path: [0, 1], kind: "event" }, { path: [0, 1, 0], kind: "text" }, { path: [0, 2], kind: "event" }, { path: [1], kind: "attribute" }, { path: [1], kind: "attribute" }, { path: [1], kind: "attribute" }, { path: [1], kind: "event" }, { path: [1], kind: "attribute" }] };
const $$form6 = { tag: "section", attributes: [{ key: "className", value: "main", way: "property" }], children: [{ tag: "input", attributes: [{ key: "className", value: "toggle-all", way: "property" }, { key: "type", value: "checkbox", way: "property" }, { key: "name", value: "toggle", way: "property" }], children: [] }, { tag: "label", attributes: [{ key: "htmlFor", value: "toggle-all", way: "property" }], children: [{ text: "Mark all as complete" }] }, { hole: 3 }], holes: [{ path: [], kind: "attribute" }, { path: [0], kind: "attribute" }, { path: [0], kind: "event" }, { path: [2], kind: "subtree" }] };
const $$form7 = { tag: "header", attributes: [{ key: "className", value: "header", way: "property" }], children: [{ tag: "h1", attributes: [], children: [{ text: "todos" }] }, { tag: "input", attributes: [{ key: "className", value: "new-todo", way: "property" }, { key: "placeholder", value: "What needs to be done?", way: "property" }, { key: "name", value: "newTodo", way: "property" }], children: [] }], holes: [{ path: [1], kind: "attribute" }, { path: [1], kind: "attribute" }, { path: [1], kind: "attribute" }, { path: [1], kind: "attribute" }] };
const $$form8 = { tag: "div", attributes: [{ key: "className", value: "todomvc-wrapper", way: "property" }, { key: "visibility", value: "hidden", way: "style" }], children: [{ tag: "section", attributes: [{ key: "className", value: "todoapp", way: "property" }], children: [{ hole: 0 }, { hole: 1 }, { hole: 2 }] }], holes: [{ path: [0, 0], kind: "subtree" }, { path: [0, 1], kind: "subtree" }, { path: [0, 2], kind: "subtree" }] };
const emptyModel = { entries: 0, visibility: "All", field: "", uid: 0 };
const init = maybeModel => [Maybe.withDefault(emptyModel, maybeModel), Platform$Cmd.none];
const newEntry = (desc, id) => ({ description: desc, completed: false, editing: false, id: id });
const update = (msg, model) => {
  if (msg === "NoOp") {
    return [model, Platform$Cmd.none];
  } else {
    if (msg === "Add") {
      return [{ ...model, uid: model.uid + 1, field: "", entries: $$String.isEmpty(model.field) ? model.entries : $append$List(model.entries, { hd: newEntry(model.field, model.uid), tl: 0 }) }, Platform$Cmd.none];
    } else {
      if (msg.TAG === "UpdateField") {
        const str = msg._0;
        return [{ ...model, field: str }, Platform$Cmd.none];
      } else {
        if (msg.TAG === "EditingEntry") {
          const id$4 = msg._0;
          const isEditing = msg._1;
          const updateEntry$3 = t$4 => {
  if (t$4.id === id$4) {
    return { ...t$4, editing: isEditing };
  } else {
    return t$4;
  }
};
          const focus = Browser$Dom.focus("todo-" + $$String.fromInt(id$4));
          return [{ ...model, entries: List.map(updateEntry$3, model.entries) }, Task.attempt($p0 => NoOp, focus)];
        } else {
          if (msg.TAG === "UpdateEntry") {
            const id$3 = msg._0;
            const task = msg._1;
            const updateEntry$2 = t$3 => {
  if (t$3.id === id$3) {
    return { ...t$3, description: task };
  } else {
    return t$3;
  }
};
            return [{ ...model, entries: List.map(updateEntry$2, model.entries) }, Platform$Cmd.none];
          } else {
            if (msg.TAG === "Delete") {
              const id$2 = msg._0;
              return [{ ...model, entries: List.filter(t$2 => t$2.id !== id$2, model.entries) }, Platform$Cmd.none];
            } else {
              if (msg === "DeleteComplete") {
                return [{ ...model, entries: List.filter($s1 => Basics.composeL(Basics.not, r => r.completed, $s1), model.entries) }, Platform$Cmd.none];
              } else {
                if (msg.TAG === "Check") {
                  const id$1 = msg._0;
                  const isCompleted$1 = msg._1;
                  const updateEntry$1 = t$1 => {
  if (t$1.id === id$1) {
    return { ...t$1, completed: isCompleted$1 };
  } else {
    return t$1;
  }
};
                  return [{ ...model, entries: List.map(updateEntry$1, model.entries) }, Platform$Cmd.none];
                } else {
                  if (msg.TAG === "CheckAll") {
                    const isCompleted = msg._0;
                    const updateEntry = t => ({ ...t, completed: isCompleted });
                    return [{ ...model, entries: List.map(updateEntry, model.entries) }, Platform$Cmd.none];
                  } else {
                    const visibility = msg._0;
                    return [{ ...model, visibility: visibility }, Platform$Cmd.none];
                  }
                }
              }
            }
          }
        }
      }
    }
  }
};
const updateWithStorage = (msg$1, model$1) => {
  const $s2 = update(msg$1, model$1);
  const newModel = $s2[0];
  const cmds = $s2[1];
  return [newModel, Platform$Cmd.batch({ hd: Dartea_browser.$$Port$outgoing("setStorage", newModel), tl: { hd: cmds, tl: 0 } })];
};
const viewControlsClear = entriesCompleted => ({ TAG: "block", form: $$form0, values: [VirtualDom.property("hidden", Json$Encode.bool(entriesCompleted === 0)), VirtualDom.on("click", VirtualDom.Normal(Json$Decode.succeed(DeleteComplete))), "Clear completed (" + ($$String.fromInt(entriesCompleted) + ")")] });
const viewControlsCount = entriesLeft => {
  const item_ = (entriesLeft === 1) ? " item" : " items";
  return { TAG: "block", form: $$form1, values: [$$String.fromInt(entriesLeft), item_ + " left"] };
};
const visibilitySwap = (uri, visibility$1, actualVisibility) => ({ TAG: "block", form: $$form2, values: [VirtualDom.on("click", VirtualDom.Normal(Json$Decode.succeed(ChangeVisibility(visibility$1)))), VirtualDom.property("href", Json$Encode.string(uri)), Html$Attributes.classList({ hd: ["selected", visibility$1 === actualVisibility], tl: 0 }), visibility$1] });
const viewControlsFilters = visibility$2 => ({ TAG: "block", form: $$form3, values: [visibilitySwap("#/", "All", visibility$2), visibilitySwap("#/active", "Active", visibility$2), visibilitySwap("#/completed", "Completed", visibility$2)] });
const viewControls = (visibility$3, entries) => {
  const entriesCompleted$1 = List.length(List.filter(r => r.completed, entries));
  const entriesLeft$1 = List.length(entries) - entriesCompleted$1;
  return { TAG: "block", form: $$form4, values: [VirtualDom.property("hidden", Json$Encode.bool(List.isEmpty(entries))), Html$Lazy.lazy(viewControlsCount, entriesLeft$1), Html$Lazy.lazy(viewControlsFilters, visibility$3), Html$Lazy.lazy(viewControlsClear, entriesCompleted$1)] };
};
const onEnter = msg$2 => {
  const isEnter = code => {
  if (code === 13) {
    return Json$Decode.succeed(msg$2);
  } else {
    return Json$Decode.fail("not ENTER");
  }
};
  return VirtualDom.on("keydown", VirtualDom.Normal(Json$Decode.andThen(isEnter, Html$Events.keyCode)));
};
const viewEntry = todo => ({ TAG: "block", form: $$form5, values: [Html$Attributes.classList({ hd: ["completed", todo.completed], tl: { hd: ["editing", todo.editing], tl: 0 } }), VirtualDom.property("checked", Json$Encode.bool(todo.completed)), VirtualDom.on("click", VirtualDom.Normal(Json$Decode.succeed(Check(todo.id, Basics.not(todo.completed))))), VirtualDom.on("dblclick", VirtualDom.Normal(Json$Decode.succeed(EditingEntry(todo.id, true)))), todo.description, VirtualDom.on("click", VirtualDom.Normal(Json$Decode.succeed(Delete(todo.id)))), VirtualDom.property("value", Json$Encode.string(todo.description)), VirtualDom.property("id", Json$Encode.string("todo-" + $$String.fromInt(todo.id))), Html$Events.onInput($s3 => UpdateEntry(todo.id, $s3)), VirtualDom.on("blur", VirtualDom.Normal(Json$Decode.succeed(EditingEntry(todo.id, false)))), onEnter(EditingEntry(todo.id, false))] });
const viewKeyedEntry = todo$1 => [$$String.fromInt(todo$1.id), Html$Lazy.lazy(viewEntry, todo$1)];
const viewEntries = (visibility$4, entries$1) => {
  const isVisible = todo$2 => {
  switch (visibility$4) {
    case "Completed":
      return todo$2.completed;
    case "Active":
      return Basics.not(todo$2.completed);
    default:
      return true;
  }
};
  const allCompleted = List.all(r => r.completed, entries$1);
  const cssVisibility = List.isEmpty(entries$1) ? "hidden" : "visible";
  return { TAG: "block", form: $$form6, values: [VirtualDom.style("visibility", cssVisibility), VirtualDom.property("checked", Json$Encode.bool(allCompleted)), VirtualDom.on("click", VirtualDom.Normal(Json$Decode.succeed(CheckAll(Basics.not(allCompleted))))), Html$Keyed.ul({ hd: VirtualDom.property("className", Json$Encode.string("todo-list")), tl: 0 }, List.map(viewKeyedEntry, List.filter(isVisible, entries$1)))] };
};
const viewInput = task$1 => ({ TAG: "block", form: $$form7, values: [VirtualDom.property("autofocus", Json$Encode.bool(true)), VirtualDom.property("value", Json$Encode.string(task$1)), Html$Events.onInput(UpdateField), onEnter(Add)] });
const view = model$2 => ({ TAG: "block", form: $$form8, values: [Html$Lazy.lazy(viewInput, model$2.field), Html$Lazy.lazy2(viewEntries, model$2.visibility, model$2.entries), Html$Lazy.lazy2(viewControls, model$2.visibility, model$2.entries)] });
const main = Browser.document({ init: init, view: model$3 => ({ title: "Elm • TodoMVC", body: { hd: view(model$3), tl: 0 } }), update: updateWithStorage, subscriptions: $p0$1 => Platform$Sub.none });
const setStorage = given => Dartea_browser.$$Port$outgoing("setStorage", given);
const Entry = ($a0, $a1, $a2, $a3) => ({ description: $a0, completed: $a1, editing: $a2, id: $a3 });
const Model = ($a0$1, $a1$1, $a2$1, $a3$1) => ({ entries: $a0$1, field: $a1$1, uid: $a2$1, visibility: $a3$1 });
export { Add, ChangeVisibility, Check, CheckAll, Delete, DeleteComplete, EditingEntry, Entry, Model, NoOp, UpdateEntry, UpdateField, emptyModel, init, main, newEntry, onEnter, setStorage, update, updateWithStorage, view, viewControls, viewControlsClear, viewControlsCount, viewControlsFilters, viewEntries, viewEntry, viewInput, viewKeyedEntry, visibilitySwap };
