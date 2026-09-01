import * as Dartea_browser from "./Dartea_browser.mjs";
import * as Basics from "./Basics.mjs";
import * as Browser from "./Browser.mjs";
import * as Browser$Dom from "./Browser.Dom.mjs";
import * as Html$Attributes from "./Html.Attributes.mjs";
import * as Html$Events from "./Html.Events.mjs";
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
const $$form0 = { tag: "button", attributes: [{ key: "className", value: "clear-completed", way: "property" }], children: [{ hole: 2 }], holes: [{ path: [], find: $$e => $$e, kind: "attribute" }, { path: [], find: $$e => $$e, kind: "event", event: "click", plain: true }, { path: [0], find: $$e => $$e.firstChild, kind: "text" }], guards: 2 };
const $$form1 = { tag: "span", attributes: [{ key: "className", value: "todo-count", way: "property" }], children: [{ tag: "strong", attributes: [], children: [{ hole: 0 }] }, { hole: 1 }], holes: [{ path: [0, 0], find: $$e => $$e.firstChild.firstChild, kind: "text" }, { path: [1], find: $$e => $$e.firstChild.nextSibling, kind: "text" }], guards: 2 };
const $$form2 = { tag: "li", attributes: [], children: [{ tag: "a", attributes: [], children: [{ hole: 3 }] }], holes: [{ path: [], find: $$e => $$e, kind: "event", event: "click", plain: true }, { path: [0], find: $$e => $$e.firstChild, kind: "slot", key: "href", way: "property" }, { path: [0], find: $$e => $$e.firstChild, kind: "attribute" }, { path: [0, 0], find: $$e => $$e.firstChild.firstChild, kind: "text" }], guards: 3 };
const $$form3 = { tag: "ul", attributes: [{ key: "className", value: "filters", way: "property" }], children: [{ hole: 0 }, { text: " " }, { hole: 1 }, { text: " " }, { hole: 2 }], holes: [{ path: [0], find: $$e => $$e.firstChild, kind: "subtree" }, { path: [2], find: $$e => $$e.firstChild.nextSibling.nextSibling, kind: "subtree" }, { path: [4], find: $$e => $$e.firstChild.nextSibling.nextSibling.nextSibling.nextSibling, kind: "subtree" }], guards: 3 };
const $$form4 = { tag: "footer", attributes: [{ key: "className", value: "footer", way: "property" }], children: [{ hole: 1 }, { hole: 2 }, { hole: 3 }], holes: [{ path: [], find: $$e => $$e, kind: "attribute" }, { path: [0], find: $$e => $$e.firstChild, kind: "subtree" }, { path: [1], find: $$e => $$e.firstChild.nextSibling, kind: "subtree" }, { path: [2], find: $$e => $$e.firstChild.nextSibling.nextSibling, kind: "subtree" }], guards: 4 };
const $$form5 = { tag: "li", attributes: [], children: [{ tag: "div", attributes: [{ key: "className", value: "view", way: "property" }], children: [{ tag: "input", attributes: [{ key: "className", value: "toggle", way: "property" }, { key: "type", value: "checkbox", way: "property" }], children: [] }, { tag: "label", attributes: [], children: [{ hole: 4 }] }, { tag: "button", attributes: [{ key: "className", value: "destroy", way: "property" }], children: [] }] }, { tag: "input", attributes: [{ key: "className", value: "edit", way: "property" }, { key: "name", value: "title", way: "property" }], children: [] }], holes: [{ path: [], find: $$e => $$e, kind: "attribute" }, { path: [0, 0], find: $$e => $$e.firstChild.firstChild, kind: "slot", key: "checked", way: "property" }, { path: [0, 0], find: $$e => $$e.firstChild.firstChild, kind: "event", event: "click", plain: true }, { path: [0, 1], find: $$e => $$e.firstChild.firstChild.nextSibling, kind: "event", event: "dblclick", plain: true }, { path: [0, 1, 0], find: $$e => $$e.firstChild.firstChild.nextSibling.firstChild, kind: "text" }, { path: [0, 2], find: $$e => $$e.firstChild.firstChild.nextSibling.nextSibling, kind: "event", event: "click", plain: true }, { path: [1], find: $$e => $$e.firstChild.nextSibling, kind: "slot", key: "value", way: "property" }, { path: [1], find: $$e => $$e.firstChild.nextSibling, kind: "slot", key: "id", way: "property" }, { path: [1], find: $$e => $$e.firstChild.nextSibling, kind: "attribute" }, { path: [1], find: $$e => $$e.firstChild.nextSibling, kind: "event", event: "blur", plain: true }, { path: [1], find: $$e => $$e.firstChild.nextSibling, kind: "attribute" }], guards: 11 };
const $$form6 = { tag: "section", attributes: [{ key: "className", value: "main", way: "property" }], children: [{ tag: "input", attributes: [{ key: "className", value: "toggle-all", way: "property" }, { key: "type", value: "checkbox", way: "property" }, { key: "name", value: "toggle", way: "property" }], children: [] }, { tag: "label", attributes: [{ key: "htmlFor", value: "toggle-all", way: "property" }], children: [{ text: "Mark all as complete" }] }, { tag: "ul", attributes: [{ key: "className", value: "todo-list", way: "property" }], children: [] }], holes: [{ path: [], find: $$e => $$e, kind: "slot", key: "visibility", way: "style" }, { path: [0], find: $$e => $$e.firstChild, kind: "slot", key: "checked", way: "property" }, { path: [0], find: $$e => $$e.firstChild, kind: "event", event: "click", plain: true }, { path: [2], find: $$e => $$e.firstChild.nextSibling.nextSibling, kind: "rows", keyed: true }], guards: 4 };
const $$form7 = { tag: "header", attributes: [{ key: "className", value: "header", way: "property" }], children: [{ tag: "h1", attributes: [], children: [{ text: "todos" }] }, { tag: "input", attributes: [{ key: "className", value: "new-todo", way: "property" }, { key: "placeholder", value: "What needs to be done?", way: "property" }, { key: "name", value: "newTodo", way: "property" }], children: [] }], holes: [{ path: [1], find: $$e => $$e.firstChild.nextSibling, kind: "attribute" }, { path: [1], find: $$e => $$e.firstChild.nextSibling, kind: "slot", key: "value", way: "property" }, { path: [1], find: $$e => $$e.firstChild.nextSibling, kind: "attribute" }, { path: [1], find: $$e => $$e.firstChild.nextSibling, kind: "attribute" }], guards: 3 };
const $$form8 = { tag: "div", attributes: [{ key: "className", value: "todomvc-wrapper", way: "property" }, { key: "visibility", value: "hidden", way: "style" }], children: [{ tag: "section", attributes: [{ key: "className", value: "todoapp", way: "property" }], children: [{ hole: 0 }, { hole: 1 }, { hole: 2 }] }], holes: [{ path: [0, 0], find: $$e => $$e.firstChild.firstChild, kind: "subtree" }, { path: [0, 1], find: $$e => $$e.firstChild.firstChild.nextSibling, kind: "subtree" }, { path: [0, 2], find: $$e => $$e.firstChild.firstChild.nextSibling.nextSibling, kind: "subtree" }], guards: 5 };
const $$r0 = ($$b, $$put, $$v) => {
  const entriesCompleted = $$v.a0;
  if ($$b.deps[0] !== entriesCompleted) {
    $$b.deps[0] = entriesCompleted;
    $$put($$b, 0, VirtualDom.property("hidden", Json$Encode.bool(entriesCompleted === 0)));
    $$put($$b, 2, "Clear completed (" + ($$String.fromInt(entriesCompleted) + ")"));
  }
  $$put($$b, 1, DeleteComplete);
};
const $$r1 = ($$b, $$put, $$v) => {
  const entriesLeft = $$v.a0;
  const item_ = $$v.a1;
  if ($$b.deps[0] !== entriesLeft) {
    $$b.deps[0] = entriesLeft;
    $$put($$b, 0, $$String.fromInt(entriesLeft));
  }
  if ($$b.deps[1] !== item_) {
    $$b.deps[1] = item_;
    $$put($$b, 1, item_ + " left");
  }
};
const $$r2 = ($$b, $$put, $$v) => {
  const actualVisibility = $$v.a0;
  const uri = $$v.a1;
  const visibility$1 = $$v.a2;
  if ($$b.deps[0] !== visibility$1) {
    $$b.deps[0] = visibility$1;
    $$put($$b, 0, ChangeVisibility(visibility$1));
  }
  $$put($$b, 1, uri);
  if (($$b.deps[1] !== visibility$1) || ($$b.deps[2] !== actualVisibility)) {
    $$b.deps[1] = visibility$1;
    $$b.deps[2] = actualVisibility;
    $$put($$b, 2, Html$Attributes.classList({ hd: ["selected", visibility$1 === actualVisibility], tl: 0 }));
  }
  $$put($$b, 3, visibility$1);
};
const $$r3 = ($$b, $$put, $$v) => {
  const visibility$2 = $$v.a0;
  if ($$b.deps[0] !== visibility$2) {
    $$b.deps[0] = visibility$2;
    $$put($$b, 0, visibilitySwap("#/", "All", visibility$2));
    $$put($$b, 1, visibilitySwap("#/active", "Active", visibility$2));
    $$put($$b, 2, visibilitySwap("#/completed", "Completed", visibility$2));
  }
};
const $$r4 = ($$b, $$put, $$v) => {
  const entries = $$v.a0;
  const entriesCompleted$1 = $$v.a1;
  const entriesLeft$1 = $$v.a2;
  const visibility$3 = $$v.a3;
  if ($$b.deps[0] !== entries) {
    $$b.deps[0] = entries;
    $$put($$b, 0, VirtualDom.property("hidden", Json$Encode.bool(List.isEmpty(entries))));
  }
  if ($$b.deps[1] !== entriesLeft$1) {
    $$b.deps[1] = entriesLeft$1;
    $$put($$b, 1, Html$Lazy.lazy(viewControlsCount, entriesLeft$1));
  }
  if ($$b.deps[2] !== visibility$3) {
    $$b.deps[2] = visibility$3;
    $$put($$b, 2, Html$Lazy.lazy(viewControlsFilters, visibility$3));
  }
  if ($$b.deps[3] !== entriesCompleted$1) {
    $$b.deps[3] = entriesCompleted$1;
    $$put($$b, 3, Html$Lazy.lazy(viewControlsClear, entriesCompleted$1));
  }
};
const $$r5 = ($$b, $$put, $$v) => {
  const todo = $$v.a0;
  if (($$b.deps[0] !== todo.completed) || ($$b.deps[1] !== todo.editing)) {
    $$b.deps[0] = todo.completed;
    $$b.deps[1] = todo.editing;
    $$put($$b, 0, Html$Attributes.classList({ hd: ["completed", todo.completed], tl: { hd: ["editing", todo.editing], tl: 0 } }));
  }
  if ($$b.deps[2] !== todo.completed) {
    $$b.deps[2] = todo.completed;
    $$put($$b, 1, Json$Encode.bool(todo.completed));
  }
  if (($$b.deps[3] !== todo.id) || ($$b.deps[4] !== todo.completed)) {
    $$b.deps[3] = todo.id;
    $$b.deps[4] = todo.completed;
    $$put($$b, 2, Check(todo.id, Basics.not(todo.completed)));
  }
  if ($$b.deps[5] !== todo.id) {
    $$b.deps[5] = todo.id;
    $$put($$b, 3, EditingEntry(todo.id, true));
    $$put($$b, 5, Delete(todo.id));
    $$put($$b, 7, "todo-" + $$String.fromInt(todo.id));
    $$put($$b, 8, Html$Events.onInput($s3 => UpdateEntry(todo.id, $s3)));
    $$put($$b, 9, EditingEntry(todo.id, false));
    $$put($$b, 10, onEnter(EditingEntry(todo.id, false)));
  }
  $$put($$b, 4, todo.description);
  $$put($$b, 6, todo.description);
};
const $$r6 = ($$b, $$put, $$v) => {
  const allCompleted = $$v.a0;
  const cssVisibility = $$v.a1;
  const entries$1 = $$v.a2;
  const isVisible = $$v.a3;
  $$put($$b, 0, cssVisibility);
  if ($$b.deps[0] !== allCompleted) {
    $$b.deps[0] = allCompleted;
    $$put($$b, 1, Json$Encode.bool(allCompleted));
    $$put($$b, 2, CheckAll(Basics.not(allCompleted)));
  }
  if (($$b.deps[1] !== isVisible) || ($$b.deps[2] !== entries$1)) {
    $$b.deps[1] = isVisible;
    $$b.deps[2] = entries$1;
    $$put($$b, 3, [viewKeyedEntry, [], List.filter(isVisible, entries$1)]);
  }
};
const $$r7 = ($$b, $$put, $$v) => {
  const task$1 = $$v.a0;
  if ($$b.deps[0] !== true) {
    $$b.deps[0] = true;
    $$put($$b, 0, VirtualDom.property("autofocus", Json$Encode.bool(true)));
    $$put($$b, 2, Html$Events.onInput(UpdateField));
    $$put($$b, 3, onEnter(Add));
  }
  $$put($$b, 1, task$1);
};
const $$r8 = ($$b, $$put, $$v) => {
  const model$2 = $$v.a0;
  if ($$b.deps[0] !== model$2.field) {
    $$b.deps[0] = model$2.field;
    $$put($$b, 0, Html$Lazy.lazy(viewInput, model$2.field));
  }
  if (($$b.deps[1] !== model$2.visibility) || ($$b.deps[2] !== model$2.entries)) {
    $$b.deps[1] = model$2.visibility;
    $$b.deps[2] = model$2.entries;
    $$put($$b, 1, Html$Lazy.lazy2(viewEntries, model$2.visibility, model$2.entries));
    $$put($$b, 2, Html$Lazy.lazy2(viewControls, model$2.visibility, model$2.entries));
  }
};
const emptyModel = { entries: 0, visibility: "All", field: "", uid: 0 };
const init = maybeModel => [Maybe.withDefault(emptyModel, maybeModel), Platform$Cmd.none];
const newEntry = (desc, id) => ({ description: desc, completed: false, editing: false, id: id });
const update = (msg, model) => {
  if (msg === "NoOp") {
    return [model, Platform$Cmd.none];
  } else {
    if (msg === "Add") {
      return [{ visibility: model.visibility, uid: model.uid + 1, field: "", entries: $$String.isEmpty(model.field) ? model.entries : $append$List(model.entries, { hd: newEntry(model.field, model.uid), tl: 0 }) }, Platform$Cmd.none];
    } else {
      if (msg.TAG === "UpdateField") {
        const str = msg._0;
        return [{ entries: model.entries, uid: model.uid, visibility: model.visibility, field: str }, Platform$Cmd.none];
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
          return [{ field: model.field, uid: model.uid, visibility: model.visibility, entries: List.map(updateEntry$3, model.entries) }, Task.attempt($p0 => NoOp, focus)];
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
            return [{ field: model.field, uid: model.uid, visibility: model.visibility, entries: List.map(updateEntry$2, model.entries) }, Platform$Cmd.none];
          } else {
            if (msg.TAG === "Delete") {
              const id$2 = msg._0;
              return [{ field: model.field, uid: model.uid, visibility: model.visibility, entries: List.filter(t$2 => t$2.id !== id$2, model.entries) }, Platform$Cmd.none];
            } else {
              if (msg === "DeleteComplete") {
                return [{ field: model.field, uid: model.uid, visibility: model.visibility, entries: List.filter($s1 => Basics.composeL(Basics.not, r => r.completed, $s1), model.entries) }, Platform$Cmd.none];
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
                  return [{ field: model.field, uid: model.uid, visibility: model.visibility, entries: List.map(updateEntry$1, model.entries) }, Platform$Cmd.none];
                } else {
                  if (msg.TAG === "CheckAll") {
                    const isCompleted = msg._0;
                    const updateEntry = t => ({ ...t, completed: isCompleted });
                    return [{ field: model.field, uid: model.uid, visibility: model.visibility, entries: List.map(updateEntry, model.entries) }, Platform$Cmd.none];
                  } else {
                    const visibility = msg._0;
                    return [{ entries: model.entries, field: model.field, uid: model.uid, visibility: visibility }, Platform$Cmd.none];
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
  return [newModel, Platform$Cmd.batch({ hd: Dartea_browser.$$Port$outgoing("setStorage", (value => ({ entries: (value => $$portList(given => given, value))(value.entries), field: value.field, uid: value.uid, visibility: value.visibility }))(newModel)), tl: { hd: cmds, tl: 0 } })];
};
const viewControlsClear = entriesCompleted => ({ TAG: "block", form: $$form0, refresh: $$r0, a0: entriesCompleted });
const viewControlsCount = entriesLeft => {
  const item_ = (entriesLeft === 1) ? " item" : " items";
  return { TAG: "block", form: $$form1, refresh: $$r1, a0: entriesLeft, a1: item_ };
};
const visibilitySwap = (uri, visibility$1, actualVisibility) => ({ TAG: "block", form: $$form2, refresh: $$r2, a0: actualVisibility, a1: uri, a2: visibility$1 });
const viewControlsFilters = visibility$2 => ({ TAG: "block", form: $$form3, refresh: $$r3, a0: visibility$2 });
const viewControls = (visibility$3, entries) => {
  const entriesCompleted$1 = List.length(List.filter(r => r.completed, entries));
  const entriesLeft$1 = List.length(entries) - entriesCompleted$1;
  return { TAG: "block", form: $$form4, refresh: $$r4, a0: entries, a1: entriesCompleted$1, a2: entriesLeft$1, a3: visibility$3 };
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
const viewEntry = todo => ({ TAG: "block", form: $$form5, refresh: $$r5, a0: todo });
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
  return { TAG: "block", form: $$form6, refresh: $$r6, a0: allCompleted, a1: cssVisibility, a2: entries$1, a3: isVisible };
};
const viewInput = task$1 => ({ TAG: "block", form: $$form7, refresh: $$r7, a0: task$1 });
const view = model$2 => ({ TAG: "block", form: $$form8, refresh: $$r8, a0: model$2 });
const main = Browser.document({ init: init, view: model$3 => ({ title: "Elm • TodoMVC", body: { hd: view(model$3), tl: 0 } }), update: updateWithStorage, subscriptions: $p0$1 => Platform$Sub.none });
const setStorage = given => Dartea_browser.$$Port$outgoing("setStorage", (value => ({ entries: (value => $$portList(given => given, value))(value.entries), field: value.field, uid: value.uid, visibility: value.visibility }))(given));
const Entry = ($a0, $a1, $a2, $a3) => ({ description: $a0, completed: $a1, editing: $a2, id: $a3 });
const Model = ($a0$1, $a1$1, $a2$1, $a3$1) => ({ entries: $a0$1, field: $a1$1, uid: $a2$1, visibility: $a3$1 });
export { Add, ChangeVisibility, Check, CheckAll, Delete, DeleteComplete, EditingEntry, Entry, Model, NoOp, UpdateEntry, UpdateField, emptyModel, init, main, newEntry, onEnter, setStorage, update, updateWithStorage, view, viewControls, viewControlsClear, viewControlsCount, viewControlsFilters, viewEntries, viewEntry, viewInput, viewKeyedEntry, visibilitySwap };
