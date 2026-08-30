const $$items = function* (list) {
  for (let rest = list; rest !== 0; rest = rest.tl) yield rest.hd;
};

const $$flagBad = (wanted, path) => {
  throw new Error("Bad flags: I was expecting " + wanted + " at " + path + ".");
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

const $$decided = (handler, event) => {
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

const $$tagged = (message, taggers) => {
  let carried = message;
  for (let chain = taggers; chain !== undefined; chain = chain.parent) {
    carried = chain.tagger(carried);
  }
  return carried;
};

const $$isPassive = (handler) => handler.TAG === "Normal" || handler.TAG === "MayStopPropagation";

const $$delivered = (app, node, held, happened) => {
  let value;
  let plain = false;
  let taggers;
  let tagger;
  if (held.b !== undefined) {
    value = held.b.held[held.index];
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
    const decided = $$decided(value, happened);
    if (decided === undefined) return true;
    if (decided.stop) {
      happened.stopPropagation();
      stop = true;
    }
    if (decided.prevent) happened.preventDefault();
    message = decided.message;
  }
  if (tagger !== undefined) message = tagger(message);
  app.dispatch($$tagged(message, taggers));
  return !stop;
};

const $$route = (app, happened) => {
  const type = happened.type;
  for (let node = happened.target; node !== null && node !== app.host; node = node.parentNode) {
    const held = node.$$on?.[type];
    if (held !== undefined && !$$delivered(app, node, held, happened)) return;
    if (!happened.bubbles) return;
  }
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

const $$matching = (attributes, attribute) =>
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

const $$erased = (element, attribute) => {
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
      const held = $$matching(before, attribute);
      if (held === undefined || held.value !== attribute.value) {
        $$wrote(element, attribute);
      }
    }
  }
  for (const attribute of before) {
    if (attribute.TAG === "on") {
      if (!$$listens(after, attribute.event)) element.$$on[attribute.event] = undefined;
    } else if ($$matching(after, attribute) === undefined) {
      $$erased(element, attribute);
    }
  }
};

const $$collapsed = (vnode) => {
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

const $$forced = (vnode) => {
  if (vnode.node === undefined) vnode.node = vnode.thunk();
  return vnode.node;
};

const $$unchanged = (before, after) =>
  before.length === after.length &&
  before.every((ref, index) => ref === after[index]);

const $$kids = (vnode) =>
  vnode.TAG === "keyed" ? vnode.children.map((pair) => pair[1]) : vnode.children;

const $$templates = new WeakMap();

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

const $$template = (form) => {
  const held = $$templates.get(form);
  if (held !== undefined) return held;
  const template = document.createElement("template");
  template.content.append($$built(form));
  $$templates.set(form, template);
  return template;
};

const $$located = (root, path) => {
  let node = root;
  for (const index of path) {
    node = node.firstChild;
    for (let step = 0; step < index; step += 1) node = node.nextSibling;
  }
  return node;
};

const $$placed = (node, fresh) => {
  node.replaceWith(fresh);
  return fresh;
};

const $$instance = (form, app, taggers) => {
  const root = $$template(form).content.firstChild.cloneNode(true);
  const nodes = form.holes.map((hole) => $$located(root, hole.path));
  const b = { form, nodes, held: [], deps: [], lists: [], app, taggers };
  root.$$block = b;
  for (const [index, hole] of form.holes.entries()) {
    if (hole.kind === "event") {
      const node = nodes[index];
      node.$$on = node.$$on ?? {};
      node.$$on[hole.event] = { b, index, plain: hole.plain };
    }
  }
  return root;
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
    if (before !== undefined) $$erased(node, before);
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

const $$entry = (keyed, at, item, made) =>
  keyed ? { key: made[0], item, vnode: made[1], node: undefined, from: -1 } : { key: at, item, vnode: made, node: undefined, from: -1 };

const $$listed = (b, index, hole, node, value) => {
  const held = b.lists[index] ?? { fn: undefined, args: [], list: undefined, entries: [] };
  const after = [];
  let fn;
  let args;
  let list;
  if (hole.kind === "children") {
    let at = 0;
    for (const child of $$items(value)) {
      const positional = held.entries[at];
      after.push(
        positional !== undefined && positional.item === child
          ? positional
          : $$entry(hole.keyed, at, child, child),
      );
      at += 1;
    }
  } else {
    [fn, args, list] = value;
    const stable = fn === held.fn && $$unchanged(args, held.args);
    if (stable && list === held.list) return;
    let byItem;
    let at = 0;
    for (const item of $$items(list)) {
      const positional = held.entries[at];
      if (stable && positional !== undefined && positional.item === item) {
        after.push(positional);
      } else {
        byItem ??= stable && hole.keyed ? new Map(held.entries.map((entry) => [entry.item, entry])) : new Map();
        const found = byItem.get(item);
        after.push(found ?? $$entry(hole.keyed, at, item, $$row(fn, args, item)));
      }
      at += 1;
    }
  }
  b.lists[index] = { fn, args, list, entries: $$rows(node, held.entries, after, hole.keyed, b.app, b.taggers) };
};

const $$slot = (node, hole, value) => {
  if (hole.way === "property") node[hole.key] = value;
  else if (hole.way === "style") node.style.setProperty(hole.key, value);
  else node.setAttribute(hole.key, value);
};

const $$put = (b, index, value) => {
  const before = b.held[index];
  if (before === value) return;
  b.held[index] = value;
  const hole = b.form.holes[index];
  const node = b.nodes[index];
  if (hole.kind === "text") {
    node.data = value;
  } else if (hole.kind === "slot") {
    $$slot(node, hole, value);
  } else if (hole.kind === "attribute") {
    $$written(b, node, before, value);
  } else if (hole.kind === "event") {
    $$delegate(b.app, hole.event, hole.plain || $$isPassive(value));
  } else if (hole.kind === "subtree") {
    b.nodes[index] =
      before === undefined
        ? $$placed(node, $$element(value, b.app, b.taggers))
        : $$patch(node.parentNode, node, before, value, b.app, b.taggers);
  } else {
    $$listed(b, index, hole, node, value);
  }
};

const $$refresh = (element, vnode, app, taggers) => {
  const b = element.$$block;
  b.app = app;
  b.taggers = taggers;
  vnode.refresh(b, $$put);
};

const $$block = (vnode, app, taggers) => {
  const element = $$instance(vnode.form, app, taggers);
  $$refresh(element, vnode, app, taggers);
  return element;
};

const $$element = (vnode, app, taggers) => {
  if (vnode.TAG === "block") return $$block(vnode, app, taggers);
  if (vnode.TAG === "map") {
    const { tagger, inside } = $$collapsed(vnode);
    const chain = { parent: taggers, tagger };
    const element = $$element(inside, app, chain);
    element.$$chain = chain;
    return element;
  }
  if (vnode.TAG === "lazy") return $$element($$forced(vnode), app, taggers);
  if (vnode.TAG === "text") return document.createTextNode(vnode.text);
  const element =
    vnode.namespace === undefined
      ? document.createElement(vnode.tag)
      : document.createElementNS(vnode.namespace, vnode.tag);
  $$attributes(element, [], vnode.attributes, app, taggers);
  element.append(...$$kids(vnode).map((child) => $$element(child, app, taggers)));
  return element;
};

const $$replaced = (before, after) =>
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

const $$moved = (parent, node, anchor) => {
  if (node.parentNode === parent && parent.moveBefore) parent.moveBefore(node, anchor);
  else parent.insertBefore(node, anchor);
};

const $$keyed = (element, before, after, app, taggers) => {
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
    if (!kept.has(index)) $$moved(element, node, anchor);
    anchor = node;
  }
};

const $$rows = (element, held, after, keyed, app, taggers) => {
  const existing = new Map();
  for (let index = 0; index < held.length; index += 1) {
    existing.set(keyed ? held[index].key : index, index);
  }
  let ordered = true;
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
      if (from < last) ordered = false;
      last = from;
    }
  }
  for (const from of existing.values()) element.removeChild(held[from].node);
  if (ordered) {
    let anchor = null;
    for (let index = after.length - 1; index >= 0; index -= 1) {
      const entry = after[index];
      if (entry.from < 0) element.insertBefore(entry.node, anchor);
      anchor = entry.node;
    }
    return after;
  }
  const kept = $$stable(after.map((entry) => entry.from));
  let anchor = null;
  for (let index = after.length - 1; index >= 0; index -= 1) {
    const node = after[index].node;
    if (!kept.has(index)) $$moved(element, node, anchor);
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
  if ($$replaced(before, after)) {
    const fresh = $$element(after, app, taggers);
    parent.replaceChild(fresh, element);
    return fresh;
  }
  if (after.TAG === "map") {
    const chain = element.$$chain;
    chain.tagger = $$collapsed(after).tagger;
    const fresh = $$patch(
      parent,
      element,
      $$collapsed(before).inside,
      $$collapsed(after).inside,
      app,
      chain,
    );
    fresh.$$chain = chain;
    return fresh;
  }
  if (after.TAG === "lazy") {
    if ($$unchanged(before.refs, after.refs)) {
      after.node = before.node;
      return element;
    }
    const fresh = $$patch(
      parent,
      element,
      $$forced(before),
      $$forced(after),
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
    $$keyed(element, before.children, after.children, app, taggers);
    return element;
  }
  $$children(element, before.children, after.children, app, taggers);
  return element;
};

const $$titled = (title) => {
  if (title !== undefined) document.title = title;
};

const $$ran = async (command, dispatch) => {
  if (command.TAG === "Batch") {
    const started = [...$$items(command._0)].map((inner) => {
      const held = [];
      return [$$ran(inner, (message) => held.push(message)), held];
    });
    for (const [done, held] of started) {
      await done;
      for (const message of held) dispatch(message);
    }
  } else if (command.TAG === "Perform") {
    await command._0(dispatch);
  }
};

const $$active = new Map();

const $$gathered = (subscription, found) => {
  if (subscription.TAG === "Batch") {
    for (const inner of $$items(subscription._0)) $$gathered(inner, found);
  } else if (subscription.TAG === "Listen") {
    found.set(subscription._0, subscription._1);
  }
  return found;
};

const $$reconciled = (subscription, dispatch) => {
  const wanted = $$gathered(subscription, new Map());
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
    $$titled(next.title);
    page = next;
  };
  const schedule = () => {
    if (pending) return;
    pending = true;
    if (drawn) deferred = true;
    else queueMicrotask(render);
  };
  const dispatch = async (msg) => {
    const [stepped, command] = program.step(msg, model);
    model = stepped;
    schedule();
    $$reconciled(program.watch(model), dispatch);
    await $$ran(command, dispatch);
  };
  const app = { dispatch, host, listened: new Map() };
  const [initial, opening] = program.start(flags, dispatch);
  model = initial;
  page = program.page(model);
  host.replaceChildren(...page.body.map((node) => $$element(node, app, undefined)));
  $$titled(page.title);
  $$reconciled(program.watch(model), dispatch);
  $$ran(opening, dispatch);
  return { ports: program.wiring() };
};
