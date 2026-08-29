import * as Dartea_browser from "./Dartea_browser.mjs";
import * as Basics from "./Basics.mjs";
import * as Browser from "./Browser.mjs";
import * as Browser$Dom from "./Browser.Dom.mjs";
import * as Html from "./Html.mjs";
import * as Html$Attributes from "./Html.Attributes.mjs";
import * as Html$Events from "./Html.Events.mjs";
import * as Html$Keyed from "./Html.Keyed.mjs";
import * as Html$Lazy from "./Html.Lazy.mjs";
import * as Json$Decode from "./Json.Decode.mjs";
import * as List from "./List.mjs";
import * as Maybe from "./Maybe.mjs";
import * as Platform$Cmd from "./Platform.Cmd.mjs";
import * as Platform$Sub from "./Platform.Sub.mjs";
import * as $$String from "./String.mjs";
import * as Task from "./Task.mjs";
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
const viewControlsClear = entriesCompleted => Html.button({ hd: Html$Attributes.$$class("clear-completed"), tl: { hd: Html$Attributes.hidden(entriesCompleted === 0), tl: { hd: Html$Events.onClick(DeleteComplete), tl: 0 } } }, { hd: Html.text("Clear completed (" + ($$String.fromInt(entriesCompleted) + ")")), tl: 0 });
const viewControlsCount = entriesLeft => {
  const item_ = (entriesLeft === 1) ? " item" : " items";
  return Html.span({ hd: Html$Attributes.$$class("todo-count"), tl: 0 }, { hd: Html.strong(0, { hd: Html.text($$String.fromInt(entriesLeft)), tl: 0 }), tl: { hd: Html.text(item_ + " left"), tl: 0 } });
};
const visibilitySwap = (uri, visibility$1, actualVisibility) => Html.li({ hd: Html$Events.onClick(ChangeVisibility(visibility$1)), tl: 0 }, { hd: Html.a({ hd: Html$Attributes.href(uri), tl: { hd: Html$Attributes.classList({ hd: ["selected", visibility$1 === actualVisibility], tl: 0 }), tl: 0 } }, { hd: Html.text(visibility$1), tl: 0 }), tl: 0 });
const viewControlsFilters = visibility$2 => Html.ul({ hd: Html$Attributes.$$class("filters"), tl: 0 }, { hd: visibilitySwap("#/", "All", visibility$2), tl: { hd: Html.text(" "), tl: { hd: visibilitySwap("#/active", "Active", visibility$2), tl: { hd: Html.text(" "), tl: { hd: visibilitySwap("#/completed", "Completed", visibility$2), tl: 0 } } } } });
const viewControls = (visibility$3, entries) => {
  const entriesCompleted$1 = List.length(List.filter(r => r.completed, entries));
  const entriesLeft$1 = List.length(entries) - entriesCompleted$1;
  return Html.footer({ hd: Html$Attributes.$$class("footer"), tl: { hd: Html$Attributes.hidden(List.isEmpty(entries)), tl: 0 } }, { hd: Html$Lazy.lazy(viewControlsCount, entriesLeft$1), tl: { hd: Html$Lazy.lazy(viewControlsFilters, visibility$3), tl: { hd: Html$Lazy.lazy(viewControlsClear, entriesCompleted$1), tl: 0 } } });
};
const onEnter = msg$2 => {
  const isEnter = code => {
  if (code === 13) {
    return Json$Decode.succeed(msg$2);
  } else {
    return Json$Decode.fail("not ENTER");
  }
};
  return Html$Events.on("keydown", Json$Decode.andThen(isEnter, Html$Events.keyCode));
};
const viewEntry = todo => Html.li({ hd: Html$Attributes.classList({ hd: ["completed", todo.completed], tl: { hd: ["editing", todo.editing], tl: 0 } }), tl: 0 }, { hd: Html.div({ hd: Html$Attributes.$$class("view"), tl: 0 }, { hd: Html.input({ hd: Html$Attributes.$$class("toggle"), tl: { hd: Html$Attributes.type_("checkbox"), tl: { hd: Html$Attributes.checked(todo.completed), tl: { hd: Html$Events.onClick(Check(todo.id, Basics.not(todo.completed))), tl: 0 } } } }, 0), tl: { hd: Html.label({ hd: Html$Events.onDoubleClick(EditingEntry(todo.id, true)), tl: 0 }, { hd: Html.text(todo.description), tl: 0 }), tl: { hd: Html.button({ hd: Html$Attributes.$$class("destroy"), tl: { hd: Html$Events.onClick(Delete(todo.id)), tl: 0 } }, 0), tl: 0 } } }), tl: { hd: Html.input({ hd: Html$Attributes.$$class("edit"), tl: { hd: Html$Attributes.value(todo.description), tl: { hd: Html$Attributes.name("title"), tl: { hd: Html$Attributes.id("todo-" + $$String.fromInt(todo.id)), tl: { hd: Html$Events.onInput($s3 => UpdateEntry(todo.id, $s3)), tl: { hd: Html$Events.onBlur(EditingEntry(todo.id, false)), tl: { hd: onEnter(EditingEntry(todo.id, false)), tl: 0 } } } } } } }, 0), tl: 0 } });
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
  return Html.section({ hd: Html$Attributes.$$class("main"), tl: { hd: Html$Attributes.style("visibility", cssVisibility), tl: 0 } }, { hd: Html.input({ hd: Html$Attributes.$$class("toggle-all"), tl: { hd: Html$Attributes.type_("checkbox"), tl: { hd: Html$Attributes.name("toggle"), tl: { hd: Html$Attributes.checked(allCompleted), tl: { hd: Html$Events.onClick(CheckAll(Basics.not(allCompleted))), tl: 0 } } } } }, 0), tl: { hd: Html.label({ hd: Html$Attributes.$$for("toggle-all"), tl: 0 }, { hd: Html.text("Mark all as complete"), tl: 0 }), tl: { hd: Html$Keyed.ul({ hd: Html$Attributes.$$class("todo-list"), tl: 0 }, List.map(viewKeyedEntry, List.filter(isVisible, entries$1))), tl: 0 } } });
};
const viewInput = task$1 => Html.header({ hd: Html$Attributes.$$class("header"), tl: 0 }, { hd: Html.h1(0, { hd: Html.text("todos"), tl: 0 }), tl: { hd: Html.input({ hd: Html$Attributes.$$class("new-todo"), tl: { hd: Html$Attributes.placeholder("What needs to be done?"), tl: { hd: Html$Attributes.autofocus(true), tl: { hd: Html$Attributes.value(task$1), tl: { hd: Html$Attributes.name("newTodo"), tl: { hd: Html$Events.onInput(UpdateField), tl: { hd: onEnter(Add), tl: 0 } } } } } } }, 0), tl: 0 } });
const view = model$2 => Html.div({ hd: Html$Attributes.$$class("todomvc-wrapper"), tl: { hd: Html$Attributes.style("visibility", "hidden"), tl: 0 } }, { hd: Html.section({ hd: Html$Attributes.$$class("todoapp"), tl: 0 }, { hd: Html$Lazy.lazy(viewInput, model$2.field), tl: { hd: Html$Lazy.lazy2(viewEntries, model$2.visibility, model$2.entries), tl: { hd: Html$Lazy.lazy2(viewControls, model$2.visibility, model$2.entries), tl: 0 } } }), tl: 0 });
const main = Browser.document({ init: init, view: model$3 => ({ title: "Elm • TodoMVC", body: { hd: view(model$3), tl: 0 } }), update: updateWithStorage, subscriptions: $p0$1 => Platform$Sub.none });
const setStorage = given => Dartea_browser.$$Port$outgoing("setStorage", given);
const Entry = ($a0, $a1, $a2, $a3) => ({ description: $a0, completed: $a1, editing: $a2, id: $a3 });
const Model = ($a0$1, $a1$1, $a2$1, $a3$1) => ({ entries: $a0$1, field: $a1$1, uid: $a2$1, visibility: $a3$1 });
export { Add, ChangeVisibility, Check, CheckAll, Delete, DeleteComplete, EditingEntry, Entry, Model, NoOp, UpdateEntry, UpdateField, emptyModel, init, main, newEntry, onEnter, setStorage, update, updateWithStorage, view, viewControls, viewControlsClear, viewControlsCount, viewControlsFilters, viewEntries, viewEntry, viewInput, viewKeyedEntry, visibilitySwap };
