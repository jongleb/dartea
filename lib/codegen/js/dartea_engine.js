const $$items = function* (list) {
  for (let rest = list; rest !== 0; rest = rest.tl) yield rest.hd;
};

const $$flagBad = (wanted, path) => {
  throw new Error("Bad flags: I was expecting " + wanted + " at " + path + ".");
};

const $$portList = (inside, list) => {
  const out = [];
  for (let rest = list; rest !== 0; rest = rest.tl) out.push(inside(rest.hd));
  return out;
};

const $$flagPrim = (value, path, fits, wanted) =>
  fits(value) ? value : $$flagBad(wanted, path);

const $$flagMaybe = (value, path, inside) =>
  value === null || value === undefined
    ? "Nothing"
    : { _0: inside(value, path) };

const $$flagList = (value, path, inside) => {
  if (!Array.isArray(value)) return $$flagBad("a LIST", path);
  let made = 0;
  for (let at = value.length - 1; at >= 0; at -= 1) {
    made = { hd: inside(value[at], path + "[" + at + "]"), tl: made };
  }
  return made;
};

const $$flagTuple = (value, path, parts) => {
  if (!Array.isArray(value) || value.length !== parts.length) {
    return $$flagBad("a TUPLE of " + parts.length, path);
  }
  return parts.map((part, at) => part(value[at], path + "[" + at + "]"));
};

const $$flagRecord = (value, path, fields) => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    return $$flagBad("an OBJECT", path);
  }
  const made = {};
  for (const name of Object.keys(fields)) {
    if (!Object.hasOwn(value, name)) {
      $$flagBad("an OBJECT with a field named `" + name + "`", path);
    }
    made[name] = fields[name](value[name], path + "." + name);
  }
  return made;
};

const $$decide = (handler, event) => {
  const decoded = handler._0._0(event);
  if (decoded.TAG !== "Ok") return undefined;
  const given = decoded._0;
  if (handler.TAG === "Normal") {
    return { message: given, stop: false, prevent: false };
  }
  if (handler.TAG === "MayStopPropagation") {
    return { message: given[0], stop: given[1], prevent: false };
  }
  if (handler.TAG === "MayPreventDefault") {
    return { message: given[0], stop: false, prevent: given[1] };
  }
  return {
    message: given.message,
    stop: given.stopPropagation,
    prevent: given.preventDefault,
  };
};

const $$wrap = (message, taggers) => {
  let carried = message;
  for (let chain = taggers; chain !== undefined; chain = chain.parent) {
    carried = chain.tagger(carried);
  }
  return carried;
};

const $$isPassive = (handler) => handler.TAG === "Normal" || handler.TAG === "MayStopPropagation";

const $$deliver = (app, node, held, happened) => {
  let value;
  let plain = false;
  let taggers;
  let tagger;
  if (held.b !== undefined) {
    value = held.b.state[held.index];
    plain = held.plain;
    taggers = held.b.taggers;
  } else {
    if (held.attribute === undefined) return true;
    value = held.attribute.handler;
    tagger = held.attribute.tagger;
    taggers = node.$$taggers;
  }
  if (value === undefined) return true;
  let message = value;
  let stop = false;
  if (!plain) {
    const decided = $$decide(value, happened);
    if (decided === undefined) return true;
    if (decided.stop) {
      happened.stopPropagation();
      stop = true;
    }
    if (decided.prevent) happened.preventDefault();
    message = decided.message;
  }
  if (tagger !== undefined) message = tagger(message);
  app.dispatch($$wrap(message, taggers));
  return !stop;
};

const $$aims = (b, root, type, happened) => {
  const holes = b.form.holes;
  let found;
  for (let index = 0; index < holes.length; index += 1) {
    const hole = holes[index];
    if (hole.kind !== "event" || hole.event !== type) continue;
    const node = (b.state[b.width + index] ??= hole.find(root));
    const fits = happened.bubbles
      ? node === happened.target || node.contains(happened.target)
      : node === happened.target;
    if (fits) (found ??= []).push({ b, index, plain: hole.plain, node });
  }
  if (found !== undefined && found.length > 1) {
    found.sort((one, other) => other.b.form.holes[other.index].path.length - one.b.form.holes[one.index].path.length);
  }
  return found;
};

const $$climb = (app, happened, from, remember) => {
  const type = happened.type;
  for (let node = from; node !== null && node !== app.host; node = node.parentNode) {
    const held = node.$$on?.[type];
    if (held !== undefined && !$$deliver(app, node, held, happened)) return;
    const b = node.$$block;
    if (b !== undefined) {
      const found = $$aims(b, node, type, happened);
      if (found !== undefined) {
        if (remember) {
          const target = happened.target;
          target.$$aim = target.$$aim ?? {};
          target.$$aim[type] = { b, root: node, found };
        }
        for (const aim of found) {
          if (!$$deliver(app, aim.node, aim, happened)) return;
        }
      }
    }
    if (!happened.bubbles && node === happened.target) return;
  }
};

const $$route = (app, happened) => {
  const target = happened.target;
  const cached = target.$$aim?.[happened.type];
  const fresh =
    cached !== undefined &&
    cached.root.isConnected &&
    cached.root.$$block === cached.b &&
    cached.root.contains(target);
  if (!fresh) {
    $$climb(app, happened, target, true);
    return;
  }
  for (const aim of cached.found) {
    if (!$$deliver(app, aim.node, aim, happened)) return;
  }
  $$climb(app, happened, cached.root.parentNode, false);
};

const $$delegate = (app, type, passive) => {
  const held = app.listened.get(type);
  if (held !== undefined && (held.passive === passive || !held.passive)) return;
  if (held !== undefined) app.host.removeEventListener(type, held.listener, { capture: true });
  const listener = (happened) => $$route(app, happened);
  app.host.addEventListener(type, listener, { capture: true, passive });
  app.listened.set(type, { listener, passive });
};

const $$listens = (attributes, event) =>
  attributes.some(
    (attribute) => attribute.TAG === "on" && attribute.event === event,
  );

const $$match = (attributes, attribute) =>
  attributes.find(
    (kept) =>
      kept.TAG === attribute.TAG &&
      kept.key === attribute.key &&
      kept.namespace === attribute.namespace,
  );

const $$wrote = (element, attribute) => {
  if (attribute.TAG === "attribute") {
    if (attribute.namespace === undefined) {
      element.setAttribute(attribute.key, attribute.value);
    } else {
      element.setAttributeNS(
        attribute.namespace,
        attribute.key,
        attribute.value,
      );
    }
  } else if (attribute.TAG === "property") {
    element[attribute.key] = attribute.value;
  } else {
    element.style.setProperty(attribute.key, attribute.value);
  }
};

const $$erase = (element, attribute) => {
  if (attribute.TAG === "attribute") {
    if (attribute.namespace === undefined) {
      element.removeAttribute(attribute.key);
    } else {
      element.removeAttributeNS(attribute.namespace, attribute.key);
    }
  } else if (attribute.TAG === "property") {
    element[attribute.key] = undefined;
  } else {
    element.style.removeProperty(attribute.key);
  }
};

const $$attributes = (element, before, after, app, taggers) => {
  element.$$on = element.$$on ?? {};
  element.$$taggers = taggers;
  for (const attribute of after) {
    if (attribute.TAG === "on") {
      $$delegate(app, attribute.event, $$isPassive(attribute.handler));
      element.$$on[attribute.event] = { attribute };
    } else {
      const held = $$match(before, attribute);
      if (held === undefined || held.value !== attribute.value) {
        $$wrote(element, attribute);
      }
    }
  }
  for (const attribute of before) {
    if (attribute.TAG === "on") {
      if (!$$listens(after, attribute.event)) element.$$on[attribute.event] = undefined;
    } else if ($$match(after, attribute) === undefined) {
      $$erase(element, attribute);
    }
  }
};

const $$flatten = (vnode) => {
  let tagger = vnode.tagger;
  let inside = vnode.node;
  while (inside.TAG === "map") {
    const outer = tagger;
    const inner = inside.tagger;
    tagger = (message) => outer(inner(message));
    inside = inside.node;
  }
  return { tagger, inside };
};

const $$force = (vnode) => {
  if (vnode.node === undefined) vnode.node = vnode.thunk();
  return vnode.node;
};

const $$same = (before, after) =>
  before.length === after.length &&
  before.every((ref, index) => ref === after[index]);

const $$kids = (vnode) =>
  vnode.TAG === "keyed" ? vnode.children.map((pair) => pair[1]) : vnode.children;

const $$blueprints = new WeakMap();

const $$fills = (hole) =>
  hole.kind === "text" || (hole.kind === "slot" && hole.way !== "property");

const $$built = (form) => {
  const element =
    form.namespace === undefined
      ? document.createElement(form.tag)
      : document.createElementNS(form.namespace, form.tag);
  for (const { key, value, way } of form.attributes) {
    $$wrote(element, { TAG: way, key, value, namespace: undefined });
  }
  element.append(
    ...form.children.map((child) =>
      child.tag === undefined ? document.createTextNode(child.text ?? "") : $$built(child),
    ),
  );
  return element;
};

const $$holdsList = (hole) => hole.kind === "rows" || hole.kind === "children";

const $$blueprint = (form) => {
  const held = $$blueprints.get(form);
  if (held !== undefined) return held;
  const template = document.createElement("template");
  template.content.append($$built(form));
  const root = template.content.firstChild;
  const made = {
    root,
    factory: form.holes.map((hole) => ($$fills(hole) ? hole.find(root) : undefined)),
    listed: form.holes.some($$holdsList),
  };
  $$blueprints.set(form, made);
  return made;
};

const $$place = (node, fresh) => {
  node.replaceWith(fresh);
  return fresh;
};


const $$written = (b, node, before, after) => {
  if (after.TAG === "on" || before?.TAG === "on") {
    $$attributes(node, before === undefined ? [] : [before], [after], b.app, b.taggers);
  } else if (
    before !== undefined &&
    before.TAG === after.TAG &&
    before.key === after.key &&
    before.namespace === after.namespace
  ) {
    if (before.value !== after.value) $$wrote(node, after);
  } else {
    if (before !== undefined) $$erase(node, before);
    $$wrote(node, after);
  }
};

const $$row = (fn, args, item) =>
  args.length === 0
    ? fn(item)
    : args.length === 1
      ? fn(args[0], item)
      : args.length === 2
        ? fn(args[0], args[1], item)
        : fn(...args, item);

const $$noEntries = [];

const $$entry = (keyed, at, item, made) =>
  keyed ? { key: made[0], item, vnode: made[1], node: undefined, from: -1 } : { key: at, item, vnode: made, node: undefined, from: -1 };

const $$rerow = (b, hole, node, fn, args, entry) => {
  const made = $$row(fn, args, entry.item);
  const vnode = hole.keyed ? made[1] : made;
  entry.node = $$patch(node, entry.node, entry.vnode, vnode, b.app, b.taggers);
  entry.vnode = vnode;
};

const $$aimed = (aims, before, after) => {
  if (aims === undefined || before.length !== after.length) return false;
  for (let at = 0; at < after.length; at += 1) {
    if (after[at] === before[at]) continue;
    if (!aims.some((aim) => aim.at === at)) return false;
  }
  return true;
};

const $$noHeld = { fn: undefined, args: $$noEntries, list: undefined, entries: $$noEntries };

const $$kept = (b, hole, node, fn, args, aims, held) => {
  if (!$$aimed(aims, held.args, args)) {
    for (const entry of held.entries) $$rerow(b, hole, node, fn, args, entry);
    return;
  }
  for (const aim of aims) {
    const before = held.args[aim.at];
    const after = args[aim.at];
    if (before === after) continue;
    for (const entry of held.entries) {
      const key = aim.get(entry.item);
      if (key === before || key === after) $$rerow(b, hole, node, fn, args, entry);
    }
  }
};

const $$childEntries = (hole, held, value) => {
  const after = [];
  let at = 0;
  for (let rest = value; rest !== 0; rest = rest.tl) {
    const child = rest.hd;
    const positional = held.entries[at];
    after.push(
      positional !== undefined && positional.item === child
        ? positional
        : $$entry(hole.keyed, at, child, child),
    );
    at += 1;
  }
  return after;
};

const $$rowEntries = (b, hole, node, fn, args, list, held, stable) => {
  const after = [];
  let byItem;
  let aligned = stable;
  let at = 0;
  for (let rest = list; rest !== 0; rest = rest.tl) {
    const item = rest.hd;
    const positional = held.entries[at];
    if (stable && positional !== undefined && positional.item === item) {
      after.push(positional);
    } else {
      const made = $$row(fn, args, item);
      if (stable && hole.keyed && positional !== undefined && positional.key === made[0]) {
        positional.item = item;
        positional.node = $$patch(node, positional.node, positional.vnode, made[1], b.app, b.taggers);
        positional.vnode = made[1];
        after.push(positional);
      } else if (stable && hole.keyed) {
        aligned = false;
        byItem ??= new Map(held.entries.map((entry) => [entry.item, entry]));
        after.push(byItem.get(item) ?? $$entry(true, at, item, made));
      } else {
        aligned = false;
        after.push($$entry(hole.keyed, at, item, made));
      }
    }
    at += 1;
  }
  if (aligned && after.length === held.entries.length) return held.entries;
  return $$rows(node, held.entries, after, hole.keyed, b.app, b.taggers);
};

const $$relist = (b, index, hole, node, value) => {
  const held = b.lists[index] ?? $$noHeld;
  if (hole.kind === "children") {
    const after = $$childEntries(hole, held, value);
    const entries = $$rows(node, held.entries, after, hole.keyed, b.app, b.taggers);
    b.lists[index] = { fn: undefined, args: $$noEntries, list: undefined, entries };
    return;
  }
  const [fn, args, list, aims] = value;
  const same = fn === held.fn;
  const stable = same && $$same(args, held.args);
  if (stable && list === held.list) return;
  if (same && list === held.list) {
    $$kept(b, hole, node, fn, args, aims, held);
    b.lists[index] = { fn, args, list, entries: held.entries };
    return;
  }
  const entries = $$rowEntries(b, hole, node, fn, args, list, held, stable);
  b.lists[index] = { fn, args, list, entries };
};

const $$slot = (node, hole, value) => {
  if (hole.way === "property") node[hole.key] = value;
  else if (hole.way === "style") node.style.setProperty(hole.key, value);
  else node.setAttribute(hole.key, value);
};

const $$apply = (b, index, value, before) => {
  const hole = b.form.holes[index];
  const node = (b.state[b.width + index] ??= hole.find(b.root));
  if (hole.kind === "text") {
    node.data = value;
  } else if (hole.kind === "slot") {
    $$slot(node, hole, value);
  } else if (hole.kind === "attribute") {
    $$written(b, node, before, value);
  } else if (hole.kind === "event") {
    $$delegate(b.app, hole.event, hole.plain || $$isPassive(value));
  } else if (hole.kind === "subtree") {
    b.state[b.width + index] =
      before === undefined
        ? $$place(node, $$element(value, b.app, b.taggers))
        : $$patch(node.parentNode, node, before, value, b.app, b.taggers);
  } else {
    $$relist(b, index, hole, node, value);
  }
};

const $$put = (b, index, value) => {
  const before = b.state[index];
  if (before === value) return;
  b.state[index] = value;
  if (b.root === undefined) {
    const hole = b.form.holes[index];
    if (hole.kind === "text") {
      b.factory[index].data = value;
    } else if ($$fills(hole)) {
      $$slot(b.factory[index], hole, value);
    } else if (hole.kind === "event") {
      $$delegate(b.app, hole.event, hole.plain || $$isPassive(value));
    } else {
      (b.pending ??= []).push(index);
    }
    return;
  }
  $$apply(b, index, value, before);
};

const $$refresh = (element, vnode, app, taggers) => {
  const b = element.$$block;
  b.app = app;
  b.taggers = taggers;
  vnode.refresh(b, $$put, vnode);
};

const $$noLists = [];

const $$slots = (count, start) => {
  const made = [];
  for (let index = 0; index < count; index += 1) made.push(start);
  return made;
};

const $$block = (vnode, app, taggers) => {
  const form = vnode.form;
  const blueprint = $$blueprint(form);
  const count = form.holes.length;
  const b = {
    form,
    root: undefined,
    factory: blueprint.factory,
    width: count,
    state: $$slots(count + count, undefined),
    deps: $$slots(form.guards, undefined),
    lists: blueprint.listed ? $$slots(count, null) : $$noLists,
    pending: undefined,
    app,
    taggers,
  };
  vnode.refresh(b, $$put, vnode);
  const root = blueprint.root.cloneNode(true);
  b.root = root;
  root.$$block = b;
  const pending = b.pending;
  b.pending = undefined;
  if (pending !== undefined) {
    for (const index of pending) $$apply(b, index, b.state[index], undefined);
  }
  return root;
};

const $$element = (vnode, app, taggers) => {
  if (vnode.TAG === "block") return $$block(vnode, app, taggers);
  if (vnode.TAG === "map") {
    const { tagger, inside } = $$flatten(vnode);
    const chain = { parent: taggers, tagger };
    const element = $$element(inside, app, chain);
    element.$$chain = chain;
    return element;
  }
  if (vnode.TAG === "lazy") return $$element($$force(vnode), app, taggers);
  if (vnode.TAG === "text") return document.createTextNode(vnode.text);
  const element =
    vnode.namespace === undefined
      ? document.createElement(vnode.tag)
      : document.createElementNS(vnode.namespace, vnode.tag);
  $$attributes(element, [], vnode.attributes, app, taggers);
  element.append(...$$kids(vnode).map((child) => $$element(child, app, taggers)));
  return element;
};

const $$differs = (before, after) =>
  before.TAG !== after.TAG ||
  (after.TAG === "block" && before.form !== after.form) ||
  ((after.TAG === "node" || after.TAG === "keyed") &&
    (before.tag !== after.tag || before.namespace !== after.namespace));

const $$stable = (positions) => {
  const tails = [];
  const previous = new Array(positions.length);
  for (const [index, position] of positions.entries()) {
    if (position < 0) continue;
    let low = 0;
    let high = tails.length;
    while (low < high) {
      const middle = (low + high) >> 1;
      if (positions[tails[middle]] < position) low = middle + 1;
      else high = middle;
    }
    previous[index] = low > 0 ? tails[low - 1] : -1;
    tails[low] = index;
  }
  const kept = new Set();
  for (let at = tails.at(-1); at !== undefined && at >= 0; at = previous[at]) kept.add(at);
  return kept;
};

const $$move = (parent, node, anchor) => {
  if (node.parentNode === parent && parent.moveBefore) parent.moveBefore(node, anchor);
  else parent.insertBefore(node, anchor);
};

const $$reorder = (element, before, after, app, taggers) => {
  const standing = [...element.childNodes];
  const existing = new Map(
    before.map(([key, vnode], index) => [key, { vnode, node: standing[index], index }]),
  );
  const placed = after.map(([key, vnode]) => {
    const held = existing.get(key);
    if (held === undefined) return { node: $$element(vnode, app, taggers), from: -1 };
    existing.delete(key);
    return { node: $$patch(element, held.node, held.vnode, vnode, app, taggers), from: held.index };
  });
  for (const held of existing.values()) element.removeChild(held.node);
  const kept = $$stable(placed.map((entry) => entry.from));
  let anchor = null;
  for (const [index, { node }] of placed.entries().toArray().toReversed()) {
    if (!kept.has(index)) $$move(element, node, anchor);
    anchor = node;
  }
};

const $$rows = (element, held, after, keyed, app, taggers) => {
  const existing = new Map();
  for (let index = 0; index < held.length; index += 1) {
    existing.set(keyed ? held[index].key : index, index);
  }
  let ordered = true;
  let kept = 0;
  let last = -1;
  for (let index = 0; index < after.length; index += 1) {
    const entry = after[index];
    const from = existing.get(keyed ? entry.key : index);
    if (from === undefined) {
      entry.node = $$element(entry.vnode, app, taggers);
      entry.from = -1;
    } else {
      existing.delete(keyed ? entry.key : index);
      const before = held[from];
      if (before !== entry) {
        entry.node = $$patch(element, before.node, before.vnode, entry.vnode, app, taggers);
      }
      entry.from = from;
      kept += 1;
      if (from < last) ordered = false;
      last = from;
    }
  }
  if (kept === 0) {
    element.replaceChildren(...after.map((entry) => entry.node));
    return after;
  }
  for (const from of existing.values()) element.removeChild(held[from].node);
  if (ordered) {
    let anchor = null;
    let batch = null;
    let batchAnchor = null;
    for (let index = after.length - 1; index >= 0; index -= 1) {
      const entry = after[index];
      if (entry.from < 0) {
        if (batch === null) {
          batch = document.createDocumentFragment();
          batchAnchor = anchor;
        }
        batch.prepend(entry.node);
      } else if (batch !== null) {
        element.insertBefore(batch, batchAnchor);
        batch = null;
      }
      anchor = entry.node;
    }
    if (batch !== null) element.insertBefore(batch, batchAnchor);
    return after;
  }
  const stable = $$stable(after.map((entry) => entry.from));
  let anchor = null;
  for (let index = after.length - 1; index >= 0; index -= 1) {
    const node = after[index].node;
    if (!stable.has(index)) $$move(element, node, anchor);
    anchor = node;
  }
  return after;
};

const $$children = (element, before, after, app, taggers) => {
  const shared = Math.min(before.length, after.length);
  for (let index = 0; index < shared; index += 1) {
    $$patch(
      element,
      element.childNodes[index],
      before[index],
      after[index],
      app,
      taggers,
    );
  }
  element.append(...after.slice(shared).map((child) => $$element(child, app, taggers)));
  for (let index = before.length - 1; index >= shared; index -= 1) {
    element.removeChild(element.childNodes[index]);
  }
};

const $$patch = (parent, element, before, after, app, taggers) => {
  if ($$differs(before, after)) {
    const fresh = $$element(after, app, taggers);
    parent.replaceChild(fresh, element);
    return fresh;
  }
  if (after.TAG === "map") {
    const chain = element.$$chain;
    chain.tagger = $$flatten(after).tagger;
    const fresh = $$patch(
      parent,
      element,
      $$flatten(before).inside,
      $$flatten(after).inside,
      app,
      chain,
    );
    fresh.$$chain = chain;
    return fresh;
  }
  if (after.TAG === "lazy") {
    if ($$same(before.refs, after.refs)) {
      after.node = before.node;
      return element;
    }
    const fresh = $$patch(
      parent,
      element,
      $$force(before),
      $$force(after),
      app,
      taggers,
    );
    return fresh;
  }
  if (after.TAG === "text") {
    if (before.text !== after.text) element.nodeValue = after.text;
    return element;
  }
  if (after.TAG === "block") {
    $$refresh(element, after, app, taggers);
    return element;
  }
  $$attributes(element, before.attributes, after.attributes, app, taggers);
  if (after.TAG === "keyed") {
    $$reorder(element, before.children, after.children, app, taggers);
    return element;
  }
  $$children(element, before.children, after.children, app, taggers);
  return element;
};

const $$entitle = (title) => {
  if (title !== undefined) document.title = title;
};

const $$ran = async (command, dispatch) => {
  if (command.TAG === "Batch") {
    const started = [];
    for (let rest = command._0; rest !== 0; rest = rest.tl) {
      const held = [];
      started.push([$$ran(rest.hd, (message) => held.push(message)), held]);
    }
    for (const [done, held] of started) {
      await done;
      for (const message of held) dispatch(message);
    }
  } else if (command.TAG === "Perform") {
    await command._0(dispatch);
  }
};

const $$idle = (command) => command.TAG !== "Batch" && command.TAG !== "Perform";

const $$active = new Map();

const $$gather = (subscription, found) => {
  if (subscription.TAG === "Batch") {
    for (const inner of $$items(subscription._0)) $$gather(inner, found);
  } else if (subscription.TAG === "Listen") {
    found.set(subscription._0, subscription._1);
  }
  return found;
};

const $$reconcile = (subscription, dispatch) => {
  const wanted = $$gather(subscription, new Map());
  for (const [key, stop] of $$active) {
    if (!wanted.has(key)) {
      stop();
      $$active.delete(key);
    }
  }
  for (const [key, watch] of wanted) {
    if (!$$active.has(key)) $$active.set(key, watch(dispatch));
  }
};

const $$frame = globalThis.requestAnimationFrame ?? ((draw) => setTimeout(draw, 16));

const $$mount = (program, node, flags) => {
  const host = program.host(node);
  let model;
  let page;
  let pending = false;
  let drawn = false;
  let deferred = false;
  const tick = () => {
    drawn = false;
    if (deferred) {
      deferred = false;
      render();
    }
  };
  const render = () => {
    pending = false;
    drawn = true;
    $$frame(tick);
    const next = program.page(model);
    $$children(host, page.body, next.body, app, undefined);
    $$entitle(next.title);
    page = next;
  };
  const schedule = () => {
    if (pending) return;
    pending = true;
    if (drawn) deferred = true;
    else queueMicrotask(render);
  };
  let watched;
  const listen = () => {
    const wanted = program.watch(model);
    if (wanted === watched) return;
    watched = wanted;
    $$reconcile(wanted, dispatch);
  };
  const dispatch = (msg) => {
    const [stepped, command] = program.step(msg, model);
    model = stepped;
    schedule();
    listen();
    if (!$$idle(command)) $$ran(command, dispatch);
  };
  const app = { dispatch, host, listened: new Map() };
  const [initial, opening] = program.start(flags, dispatch);
  model = initial;
  page = program.page(model);
  host.replaceChildren(...page.body.map((node) => $$element(node, app, undefined)));
  $$entitle(page.title);
  listen();
  if (!$$idle(opening)) $$ran(opening, dispatch);
  return { ports: program.wiring() };
};
