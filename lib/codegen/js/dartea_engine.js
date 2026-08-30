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

const $$listen = (element, event, dispatch, passive) => {
  const listener = (happened) => {
    const attribute = element.$$handlers[event]?.attribute;
    if (attribute === undefined) return;
    const decided = $$decided(attribute.handler, happened);
    if (decided === undefined) return;
    if (decided.stop) happened.stopPropagation();
    if (decided.prevent) happened.preventDefault();
    const message =
      attribute.tagger === undefined
        ? decided.message
        : attribute.tagger(decided.message);
    dispatch($$tagged(message, element.$$taggers));
  };
  element.addEventListener(event, listener, { passive });
  return { listener, passive };
};

const $$unlisten = (element, event) => {
  const held = element.$$handlers[event];
  if (held !== undefined) element.removeEventListener(event, held.listener);
  element.$$handlers[event] = undefined;
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

const $$attributes = (element, before, after, dispatch, taggers) => {
  element.$$handlers = element.$$handlers ?? {};
  element.$$taggers = taggers;
  for (const attribute of after) {
    if (attribute.TAG === "on") {
      const passive = $$isPassive(attribute.handler);
      const held = element.$$handlers[attribute.event];
      if (held?.passive !== passive) {
        $$unlisten(element, attribute.event);
        element.$$handlers[attribute.event] = $$listen(element, attribute.event, dispatch, passive);
      }
      element.$$handlers[attribute.event].attribute = attribute;
    } else {
      const held = $$matching(before, attribute);
      if (held === undefined || held.value !== attribute.value) {
        $$wrote(element, attribute);
      }
    }
  }
  for (const attribute of before) {
    if (attribute.TAG === "on") {
      if (!$$listens(after, attribute.event)) $$unlisten(element, attribute.event);
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

const $$element = (vnode, dispatch, taggers) => {
  if (vnode.TAG === "map") {
    const { tagger, inside } = $$collapsed(vnode);
    const chain = { parent: taggers, tagger };
    const element = $$element(inside, dispatch, chain);
    element.$$chain = chain;
    return element;
  }
  if (vnode.TAG === "lazy") return $$element($$forced(vnode), dispatch, taggers);
  if (vnode.TAG === "text") return document.createTextNode(vnode.text);
  const element =
    vnode.namespace === undefined
      ? document.createElement(vnode.tag)
      : document.createElementNS(vnode.namespace, vnode.tag);
  $$attributes(element, [], vnode.attributes, dispatch, taggers);
  element.append(...$$kids(vnode).map((child) => $$element(child, dispatch, taggers)));
  return element;
};

const $$replaced = (before, after) =>
  before.TAG !== after.TAG ||
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

const $$keyed = (element, before, after, dispatch, taggers) => {
  const standing = [...element.childNodes];
  const existing = new Map(
    before.map(([key, vnode], index) => [key, { vnode, node: standing[index], index }]),
  );
  const placed = after.map(([key, vnode]) => {
    const held = existing.get(key);
    if (held === undefined) return { node: $$element(vnode, dispatch, taggers), from: -1 };
    existing.delete(key);
    return { node: $$patch(element, held.node, held.vnode, vnode, dispatch, taggers), from: held.index };
  });
  for (const held of existing.values()) element.removeChild(held.node);
  const kept = $$stable(placed.map((entry) => entry.from));
  let anchor = null;
  for (const [index, { node }] of placed.entries().toArray().toReversed()) {
    if (!kept.has(index)) $$moved(element, node, anchor);
    anchor = node;
  }
};

const $$children = (element, before, after, dispatch, taggers) => {
  const shared = Math.min(before.length, after.length);
  for (let index = 0; index < shared; index += 1) {
    $$patch(
      element,
      element.childNodes[index],
      before[index],
      after[index],
      dispatch,
      taggers,
    );
  }
  element.append(...after.slice(shared).map((child) => $$element(child, dispatch, taggers)));
  for (let index = before.length - 1; index >= shared; index -= 1) {
    element.removeChild(element.childNodes[index]);
  }
};

const $$patch = (parent, element, before, after, dispatch, taggers) => {
  if ($$replaced(before, after)) {
    const fresh = $$element(after, dispatch, taggers);
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
      dispatch,
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
      dispatch,
      taggers,
    );
    return fresh;
  }
  if (after.TAG === "text") {
    if (before.text !== after.text) element.nodeValue = after.text;
    return element;
  }
  $$attributes(element, before.attributes, after.attributes, dispatch, taggers);
  if (after.TAG === "keyed") {
    $$keyed(element, before.children, after.children, dispatch, taggers);
    return element;
  }
  $$children(element, before.children, after.children, dispatch, taggers);
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
  let scheduled = false;
  const render = () => {
    scheduled = false;
    const next = program.page(model);
    $$children(host, page.body, next.body, dispatch, undefined);
    $$titled(next.title);
    page = next;
  };
  const dispatch = async (msg) => {
    const [stepped, command] = program.step(msg, model);
    model = stepped;
    if (!scheduled) {
      scheduled = true;
      $$frame(render);
    }
    $$reconciled(program.watch(model), dispatch);
    await $$ran(command, dispatch);
  };
  const [initial, opening] = program.start(flags, dispatch);
  model = initial;
  page = program.page(model);
  host.replaceChildren(...page.body.map((node) => $$element(node, dispatch, undefined)));
  $$titled(page.title);
  $$reconciled(program.watch(model), dispatch);
  $$ran(opening, dispatch);
  return { ports: program.wiring() };
};
