import * as Basics from "./Basics.mjs";
import * as Browser from "./Browser.mjs";
import * as List from "./List.mjs";
import * as Maybe from "./Maybe.mjs";
import * as Platform$Cmd from "./Platform.Cmd.mjs";
import * as Platform$Sub from "./Platform.Sub.mjs";
import * as $$String from "./String.mjs";
const Create = _0 => ({ TAG: "Create", _0: _0 });
const Append = _0 => ({ TAG: "Append", _0: _0 });
const Update = "Update";
const Clear = "Clear";
const Swap = "Swap";
const Select = _0 => ({ TAG: "Select", _0: _0 });
const Remove = _0 => ({ TAG: "Remove", _0: _0 });
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
const $$form0 = { tag: "tr", attributes: [], children: [{ tag: "td", attributes: [{ key: "className", value: "col-md-1", way: "property" }], children: [{ hole: 1 }] }, { tag: "td", attributes: [{ key: "className", value: "col-md-4", way: "property" }], children: [{ tag: "a", attributes: [], children: [{ hole: 3 }] }] }, { tag: "td", attributes: [{ key: "className", value: "col-md-1", way: "property" }], children: [{ tag: "button", attributes: [{ key: "className", value: "remove", way: "property" }], children: [{ text: "x" }] }] }, { tag: "td", attributes: [{ key: "className", value: "col-md-6", way: "property" }], children: [] }], holes: [{ path: [], find: $$e => $$e, kind: "slot", key: "className", way: "property" }, { path: [0, 0], find: $$e => $$e.firstChild.firstChild, kind: "text" }, { path: [1, 0], find: $$e => $$e.firstChild.nextSibling.firstChild, kind: "event", event: "click", plain: true }, { path: [1, 0, 0], find: $$e => $$e.firstChild.nextSibling.firstChild.firstChild, kind: "text" }, { path: [2, 0], find: $$e => $$e.firstChild.nextSibling.nextSibling.firstChild, kind: "event", event: "click", plain: true }] };
const $$form1 = { tag: "div", attributes: [{ key: "className", value: "container", way: "property" }], children: [{ tag: "div", attributes: [{ key: "className", value: "jumbotron", way: "property" }], children: [{ tag: "h1", attributes: [], children: [{ text: "dartea keyed" }] }, { tag: "button", attributes: [{ key: "id", value: "run", way: "property" }], children: [{ text: "Create 1,000 rows" }] }, { tag: "button", attributes: [{ key: "id", value: "runlots", way: "property" }], children: [{ text: "Create 10,000 rows" }] }, { tag: "button", attributes: [{ key: "id", value: "add", way: "property" }], children: [{ text: "Append 1,000 rows" }] }, { tag: "button", attributes: [{ key: "id", value: "update", way: "property" }], children: [{ text: "Update every 10th row" }] }, { tag: "button", attributes: [{ key: "id", value: "clear", way: "property" }], children: [{ text: "Clear" }] }, { tag: "button", attributes: [{ key: "id", value: "swaprows", way: "property" }], children: [{ text: "Swap Rows" }] }] }, { tag: "table", attributes: [{ key: "className", value: "table", way: "property" }], children: [{ tag: "tbody", attributes: [], children: [] }] }], holes: [{ path: [0, 1], find: $$e => $$e.firstChild.firstChild.nextSibling, kind: "event", event: "click", plain: true }, { path: [0, 2], find: $$e => $$e.firstChild.firstChild.nextSibling.nextSibling, kind: "event", event: "click", plain: true }, { path: [0, 3], find: $$e => $$e.firstChild.firstChild.nextSibling.nextSibling.nextSibling, kind: "event", event: "click", plain: true }, { path: [0, 4], find: $$e => $$e.firstChild.firstChild.nextSibling.nextSibling.nextSibling.nextSibling, kind: "event", event: "click", plain: true }, { path: [0, 5], find: $$e => $$e.firstChild.firstChild.nextSibling.nextSibling.nextSibling.nextSibling.nextSibling, kind: "event", event: "click", plain: true }, { path: [0, 6], find: $$e => $$e.firstChild.firstChild.nextSibling.nextSibling.nextSibling.nextSibling.nextSibling.nextSibling, kind: "event", event: "click", plain: true }, { path: [1, 0], find: $$e => $$e.firstChild.nextSibling.firstChild, kind: "rows", keyed: true }] };
const $$noargs = [];
const $$r0 = ($$b, $$put, $$a) => {
  const row$3 = $$a[0];
  const selected = $$a[1];
  $$put($$b, 0, (selected === row$3.id) ? "danger" : "");
  if ($$b.deps[1] !== row$3.id) {
    $$b.deps[1] = row$3.id;
    $$put($$b, 1, $$String.fromInt(row$3.id));
  }
  if ($$b.deps[2] !== row$3.id) {
    $$b.deps[2] = row$3.id;
    $$put($$b, 2, Select(row$3.id));
  }
  $$put($$b, 3, row$3.label);
  if ($$b.deps[4] !== row$3.id) {
    $$b.deps[4] = row$3.id;
    $$put($$b, 4, Remove(row$3.id));
  }
};
const $$r1 = ($$b, $$put, $$a) => {
  const model$2 = $$a[0];
  if ($$b.deps[0] !== true) {
    $$b.deps[0] = true;
    $$put($$b, 0, Create(1000));
  }
  if ($$b.deps[1] !== true) {
    $$b.deps[1] = true;
    $$put($$b, 1, Create(10000));
  }
  if ($$b.deps[2] !== true) {
    $$b.deps[2] = true;
    $$put($$b, 2, Append(1000));
  }
  $$put($$b, 3, Update);
  $$put($$b, 4, Clear);
  $$put($$b, 5, Swap);
  $$put($$b, 6, [viewRow, [model$2.selected], model$2.rows]);
};
const adjectives = { hd: "pretty", tl: { hd: "large", tl: { hd: "big", tl: { hd: "small", tl: { hd: "tall", tl: { hd: "short", tl: { hd: "long", tl: { hd: "handsome", tl: { hd: "plain", tl: { hd: "quaint", tl: { hd: "clean", tl: { hd: "elegant", tl: { hd: "easy", tl: { hd: "angry", tl: { hd: "crazy", tl: { hd: "helpful", tl: { hd: "mushy", tl: { hd: "odd", tl: { hd: "unsightly", tl: { hd: "adorable", tl: { hd: "important", tl: { hd: "inexpensive", tl: { hd: "cheap", tl: { hd: "expensive", tl: { hd: "fancy", tl: 0 } } } } } } } } } } } } } } } } } } } } } } } } };
const colours = { hd: "red", tl: { hd: "yellow", tl: { hd: "blue", tl: { hd: "green", tl: { hd: "pink", tl: { hd: "brown", tl: { hd: "purple", tl: { hd: "brown", tl: { hd: "white", tl: { hd: "black", tl: { hd: "orange", tl: 0 } } } } } } } } } } };
const nouns = { hd: "table", tl: { hd: "chair", tl: { hd: "house", tl: { hd: "bbq", tl: { hd: "desk", tl: { hd: "car", tl: { hd: "pony", tl: { hd: "cookie", tl: { hd: "sandwich", tl: { hd: "burger", tl: { hd: "pizza", tl: { hd: "mouse", tl: { hd: "keyboard", tl: 0 } } } } } } } } } } } } };
const pick = (seed, count, words) => Maybe.withDefault("", List.head(List.drop(Basics.modBy(count, seed), words)));
const label = seed$1 => pick(seed$1, 25, adjectives) + (" " + (pick((seed$1 / 7) | 0, 11, colours) + (" " + pick((seed$1 / 3) | 0, 13, nouns))));
const growth = (left, rowId, seed$2, rows) => {
  while (true) {
    if (left === 0) {
      return [List.reverse(rows), [rowId, seed$2]];
    } else {
      const $s1 = left - 1;
      const $s2 = rowId + 1;
      const $s3 = seed$2 + 13;
      const $s4 = { hd: { id: rowId, label: label(seed$2) }, tl: rows };
      left = $s1;
      rowId = $s2;
      seed$2 = $s3;
      rows = $s4;
      continue;
    }
  }
};
const build = (count$1, model) => {
  const $s5 = growth(count$1, model.next, model.seed, 0);
  const next = $s5[1][0];
  const seed$3 = $s5[1][1];
  const rows$1 = $s5[0];
  return [rows$1, { rows: model.rows, selected: model.selected, next: next, seed: seed$3 }];
};
const init = $p0 => [{ rows: 0, selected: 0, next: 1, seed: 0 }, Platform$Cmd.none];
const bump = (index, row) => {
  if (Basics.modBy(10, index) === 0) {
    return { id: row.id, label: row.label + " !!!" };
  } else {
    return row;
  }
};
const swap = rows$2 => {
  const at = index$1 => List.head(List.drop(index$1, rows$2));
  const $s6 = [at(1), at(998)];
  if (typeof $s6[0] === "object") {
    if (typeof $s6[1] === "object") {
      const target = $s6[1]._0;
      const second = $s6[0]._0;
      return List.indexedMap((index$2, row$1) => {
  if (index$2 === 1) {
    return target;
  } else {
    if (index$2 === 998) {
      return second;
    } else {
      return row$1;
    }
  }
}, rows$2);
    } else {
      return rows$2;
    }
  } else {
    return rows$2;
  }
};
const update = (msg, model$1) => {
  if (msg.TAG === "Create") {
    const count$3 = msg._0;
    const $s8 = build(count$3, model$1);
    const rows$4 = $s8[0];
    const next$2 = $s8[1];
    return [{ selected: next$2.selected, next: next$2.next, seed: next$2.seed, rows: rows$4 }, Platform$Cmd.none];
  } else {
    if (msg.TAG === "Append") {
      const count$2 = msg._0;
      const $s7 = build(count$2, model$1);
      const rows$3 = $s7[0];
      const next$1 = $s7[1];
      return [{ selected: next$1.selected, next: next$1.next, seed: next$1.seed, rows: $append$List(model$1.rows, rows$3) }, Platform$Cmd.none];
    } else {
      if (msg === "Update") {
        return [{ selected: model$1.selected, next: model$1.next, seed: model$1.seed, rows: List.indexedMap(bump, model$1.rows) }, Platform$Cmd.none];
      } else {
        if (msg === "Clear") {
          return [{ selected: model$1.selected, next: model$1.next, seed: model$1.seed, rows: 0 }, Platform$Cmd.none];
        } else {
          if (msg === "Swap") {
            return [{ selected: model$1.selected, next: model$1.next, seed: model$1.seed, rows: swap(model$1.rows) }, Platform$Cmd.none];
          } else {
            if (msg.TAG === "Select") {
              const rowId$2 = msg._0;
              return [{ rows: model$1.rows, next: model$1.next, seed: model$1.seed, selected: rowId$2 }, Platform$Cmd.none];
            } else {
              const rowId$1 = msg._0;
              return [{ selected: model$1.selected, next: model$1.next, seed: model$1.seed, rows: List.filter(row$2 => row$2.id !== rowId$1, model$1.rows) }, Platform$Cmd.none];
            }
          }
        }
      }
    }
  }
};
const viewRow = (selected, row$3) => [$$String.fromInt(row$3.id), { TAG: "block", form: $$form0, refresh: $$r0, args: [row$3, selected] }];
const view = model$2 => ({ TAG: "block", form: $$form1, refresh: $$r1, args: [model$2] });
const main = Browser.element({ init: init, view: view, update: update, subscriptions: $p0$1 => Platform$Sub.none });
export { main };
