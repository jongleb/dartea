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
const $$form0 = { tag: "button", attributes: [{ key: "className", value: "clear-completed", way: "property" }], children: [{ hole: 2 }], holes: [{ path: [], kind: "attribute" }, { path: [], kind: "event", event: "click", plain: true }, { path: [0], kind: "text" }] };
const $$form1 = { tag: "span", attributes: [{ key: "className", value: "todo-count", way: "property" }], children: [{ tag: "strong", attributes: [], children: [{ hole: 0 }] }, { hole: 1 }], holes: [{ path: [0, 0], kind: "text" }, { path: [1], kind: "text" }] };
const $$form2 = { tag: "li", attributes: [], children: [{ tag: "a", attributes: [], children: [{ hole: 3 }] }], holes: [{ path: [], kind: "event", event: "click", plain: true }, { path: [0], kind: "slot", key: "href", way: "property" }, { path: [0], kind: "attribute" }, { path: [0, 0], kind: "text" }] };
const $$form3 = { tag: "ul", attributes: [{ key: "className", value: "filters", way: "property" }], children: [{ hole: 0 }, { text: " " }, { hole: 1 }, { text: " " }, { hole: 2 }], holes: [{ path: [0], kind: "subtree" }, { path: [2], kind: "subtree" }, { path: [4], kind: "subtree" }] };
const $$form4 = { tag: "footer", attributes: [{ key: "className", value: "footer", way: "property" }], children: [{ hole: 1 }, { hole: 2 }, { hole: 3 }], holes: [{ path: [], kind: "attribute" }, { path: [0], kind: "subtree" }, { path: [1], kind: "subtree" }, { path: [2], kind: "subtree" }] };
const $$form5 = { tag: "li", attributes: [], children: [{ tag: "div", attributes: [{ key: "className", value: "view", way: "property" }], children: [{ tag: "input", attributes: [{ key: "className", value: "toggle", way: "property" }, { key: "type", value: "checkbox", way: "property" }], children: [] }, { tag: "label", attributes: [], children: [{ hole: 4 }] }, { tag: "button", attributes: [{ key: "className", value: "destroy", way: "property" }], children: [] }] }, { tag: "input", attributes: [{ key: "className", value: "edit", way: "property" }, { key: "name", value: "title", way: "property" }], children: [] }], holes: [{ path: [], kind: "attribute" }, { path: [0, 0], kind: "slot", key: "checked", way: "property" }, { path: [0, 0], kind: "event", event: "click", plain: true }, { path: [0, 1], kind: "event", event: "dblclick", plain: true }, { path: [0, 1, 0], kind: "text" }, { path: [0, 2], kind: "event", event: "click", plain: true }, { path: [1], kind: "slot", key: "value", way: "property" }, { path: [1], kind: "slot", key: "id", way: "property" }, { path: [1], kind: "attribute" }, { path: [1], kind: "event", event: "blur", plain: true }, { path: [1], kind: "attribute" }] };
const $$form6 = { tag: "section", attributes: [{ key: "className", value: "main", way: "property" }], children: [{ tag: "input", attributes: [{ key: "className", value: "toggle-all", way: "property" }, { key: "type", value: "checkbox", way: "property" }, { key: "name", value: "toggle", way: "property" }], children: [] }, { tag: "label", attributes: [{ key: "htmlFor", value: "toggle-all", way: "property" }], children: [{ text: "Mark all as complete" }] }, { tag: "ul", attributes: [{ key: "className", value: "todo-list", way: "property" }], children: [] }], holes: [{ path: [], kind: "slot", key: "visibility", way: "style" }, { path: [0], kind: "slot", key: "checked", way: "property" }, { path: [0], kind: "event", event: "click", plain: true }, { path: [2], kind: "rows", keyed: true }] };
const $$form7 = { tag: "header", attributes: [{ key: "className", value: "header", way: "property" }], children: [{ tag: "h1", attributes: [], children: [{ text: "todos" }] }, { tag: "input", attributes: [{ key: "className", value: "new-todo", way: "property" }, { key: "placeholder", value: "What needs to be done?", way: "property" }, { key: "name", value: "newTodo", way: "property" }], children: [] }], holes: [{ path: [1], kind: "attribute" }, { path: [1], kind: "slot", key: "value", way: "property" }, { path: [1], kind: "attribute" }, { path: [1], kind: "attribute" }] };
const $$form8 = { tag: "div", attributes: [{ key: "className", value: "todomvc-wrapper", way: "property" }, { key: "visibility", value: "hidden", way: "style" }], children: [{ tag: "section", attributes: [{ key: "className", value: "todoapp", way: "property" }], children: [{ hole: 0 }, { hole: 1 }, { hole: 2 }] }], holes: [{ path: [0, 0], kind: "subtree" }, { path: [0, 1], kind: "subtree" }, { path: [0, 2], kind: "subtree" }] };
const $$r0 = ($$b, $$put, $$a) => {
  const entriesCompleted = $$a[0];
  if ($$b.deps[0] !== entriesCompleted) {
    $$b.deps[0] = entriesCompleted;
    $$put($$b, 0, VirtualDom.property("hidden", Json$Encode.bool(entriesCompleted === 0)));
  }
  $$put($$b, 1, DeleteComplete);
  if ($$b.deps[2] !== entriesCompleted) {
    $$b.deps[2] = entriesCompleted;
    $$put($$b, 2, "Clear completed (" + ($$String.fromInt(entriesCompleted) + ")"));
  }
};
const $$r1 = ($$b, $$put, $$a) => {
  const entriesLeft = $$a[0];
  const item_ = $$a[1];
  if ($$b.deps[0] !== entriesLeft) {
    $$b.deps[0] = entriesLeft;
    $$put($$b, 0, $$String.fromInt(entriesLeft));
  }
  if ($$b.deps[1] !== item_) {
    $$b.deps[1] = item_;
    $$put($$b, 1, item_ + " left");
  }
};
const $$r2 = ($$b, $$put, $$a) => {
  const actualVisibility = $$a[0];
  const uri = $$a[1];
  const visibility$1 = $$a[2];
  if ($$b.deps[0] !== visibility$1) {
    $$b.deps[0] = visibility$1;
    $$put($$b, 0, ChangeVisibility(visibility$1));
  }
  $$put($$b, 1, uri);
  $$put($$b, 2, Html$Attributes.classList({ hd: ["selected", visibility$1 === actualVisibility], tl: 0 }));
  $$put($$b, 3, visibility$1);
};
const $$r3 = ($$b, $$put, $$a) => {
  const visibility$2 = $$a[0];
  if ($$b.deps[0] !== visibility$2) {
    $$b.deps[0] = visibility$2;
    $$put($$b, 0, visibilitySwap("#/", "All", visibility$2));
  }
  if ($$b.deps[1] !== visibility$2) {
    $$b.deps[1] = visibility$2;
    $$put($$b, 1, visibilitySwap("#/active", "Active", visibility$2));
  }
  if ($$b.deps[2] !== visibility$2) {
    $$b.deps[2] = visibility$2;
    $$put($$b, 2, visibilitySwap("#/completed", "Completed", visibility$2));
  }
};
const $$r4 = ($$b, $$put, $$a) => {
  const entries = $$a[0];
  const entriesCompleted$1 = $$a[1];
  const entriesLeft$1 = $$a[2];
  const visibility$3 = $$a[3];
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
const $$r5 = ($$b, $$put, $$a) => {
  const todo = $$a[0];
  $$put($$b, 0, Html$Attributes.classList({ hd: ["completed", todo.completed], tl: { hd: ["editing", todo.editing], tl: 0 } }));
  if ($$b.deps[1] !== todo.completed) {
    $$b.deps[1] = todo.completed;
    $$put($$b, 1, Json$Encode.bool(todo.completed));
  }
  $$put($$b, 2, Check(todo.id, Basics.not(todo.completed)));
  if ($$b.deps[3] !== todo.id) {
    $$b.deps[3] = todo.id;
    $$put($$b, 3, EditingEntry(todo.id, true));
  }
  $$put($$b, 4, todo.description);
  if ($$b.deps[5] !== todo.id) {
    $$b.deps[5] = todo.id;
    $$put($$b, 5, Delete(todo.id));
  }
  $$put($$b, 6, todo.description);
  if ($$b.deps[7] !== todo.id) {
    $$b.deps[7] = todo.id;
    $$put($$b, 7, "todo-" + $$String.fromInt(todo.id));
  }
  if ($$b.deps[8] !== todo.id) {
    $$b.deps[8] = todo.id;
    $$put($$b, 8, Html$Events.onInput($s3 => UpdateEntry(todo.id, $s3)));
  }
  if ($$b.deps[9] !== todo.id) {
    $$b.deps[9] = todo.id;
    $$put($$b, 9, EditingEntry(todo.id, false));
  }
  if ($$b.deps[10] !== todo.id) {
    $$b.deps[10] = todo.id;
    $$put($$b, 10, onEnter(EditingEntry(todo.id, false)));
  }
};
const $$r6 = ($$b, $$put, $$a) => {
  const allCompleted = $$a[0];
  const cssVisibility = $$a[1];
  const entries$1 = $$a[2];
  const isVisible = $$a[3];
  $$put($$b, 0, cssVisibility);
  if ($$b.deps[1] !== allCompleted) {
    $$b.deps[1] = allCompleted;
    $$put($$b, 1, Json$Encode.bool(allCompleted));
  }
  if ($$b.deps[2] !== allCompleted) {
    $$b.deps[2] = allCompleted;
    $$put($$b, 2, CheckAll(Basics.not(allCompleted)));
  }
  $$put($$b, 3, [viewKeyedEntry, [], List.filter(isVisible, entries$1)]);
};
const $$r7 = ($$b, $$put, $$a) => {
  const task$1 = $$a[0];
  if ($$b.deps[0] !== true) {
    $$b.deps[0] = true;
    $$put($$b, 0, VirtualDom.property("autofocus", Json$Encode.bool(true)));
  }
  $$put($$b, 1, task$1);
  if ($$b.deps[2] !== true) {
    $$b.deps[2] = true;
    $$put($$b, 2, Html$Events.onInput(UpdateField));
  }
  if ($$b.deps[3] !== true) {
    $$b.deps[3] = true;
    $$put($$b, 3, onEnter(Add));
  }
};
const $$r8 = ($$b, $$put, $$a) => {
  const model$2 = $$a[0];
  if ($$b.deps[0] !== model$2.field) {
    $$b.deps[0] = model$2.field;
    $$put($$b, 0, Html$Lazy.lazy(viewInput, model$2.field));
  }
  $$put($$b, 1, Html$Lazy.lazy2(viewEntries, model$2.visibility, model$2.entries));
  $$put($$b, 2, Html$Lazy.lazy2(viewControls, model$2.visibility, model$2.entries));
};
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
const viewControlsClear = entriesCompleted => ({ TAG: "block", form: $$form0, refresh: $$r0, args: [entriesCompleted] });
const viewControlsCount = entriesLeft => {
  const item_ = (entriesLeft === 1) ? " item" : " items";
  return { TAG: "block", form: $$form1, refresh: $$r1, args: [entriesLeft, item_] };
};
const visibilitySwap = (uri, visibility$1, actualVisibility) => ({ TAG: "block", form: $$form2, refresh: $$r2, args: [actualVisibility, uri, visibility$1] });
const viewControlsFilters = visibility$2 => ({ TAG: "block", form: $$form3, refresh: $$r3, args: [visibility$2] });
const viewControls = (visibility$3, entries) => {
  const entriesCompleted$1 = List.length(List.filter(r => r.completed, entries));
  const entriesLeft$1 = List.length(entries) - entriesCompleted$1;
  return { TAG: "block", form: $$form4, refresh: $$r4, args: [entries, entriesCompleted$1, entriesLeft$1, visibility$3] };
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
const viewEntry = todo => ({ TAG: "block", form: $$form5, refresh: $$r5, args: [todo] });
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
  return { TAG: "block", form: $$form6, refresh: $$r6, args: [allCompleted, cssVisibility, entries$1, isVisible] };
};
const viewInput = task$1 => ({ TAG: "block", form: $$form7, refresh: $$r7, args: [task$1] });
const view = model$2 => ({ TAG: "block", form: $$form8, refresh: $$r8, args: [model$2] });
const main = Browser.document({ init: init, view: model$3 => ({ title: "Elm • TodoMVC", body: { hd: view(model$3), tl: 0 } }), update: updateWithStorage, subscriptions: $p0$1 => Platform$Sub.none });
const setStorage = given => Dartea_browser.$$Port$outgoing("setStorage", given);
const Entry = ($a0, $a1, $a2, $a3) => ({ description: $a0, completed: $a1, editing: $a2, id: $a3 });
const Model = ($a0$1, $a1$1, $a2$1, $a3$1) => ({ entries: $a0$1, field: $a1$1, uid: $a2$1, visibility: $a3$1 });
export { Add, ChangeVisibility, Check, CheckAll, Delete, DeleteComplete, EditingEntry, Entry, Model, NoOp, UpdateEntry, UpdateField, emptyModel, init, main, newEntry, onEnter, setStorage, update, updateWithStorage, view, viewControls, viewControlsClear, viewControlsCount, viewControlsFilters, viewEntries, viewEntry, viewInput, viewKeyedEntry, visibilitySwap };
