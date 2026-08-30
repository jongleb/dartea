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
  for (const name in fields) {
    if (!(name in value)) {
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

const $$listen = (element, event, dispatch) => {
  element.addEventListener(event, (happened) => {
    const attribute = element.$$handlers[event];
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
  });
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
      if (element.$$handlers[attribute.event] === undefined) {
        $$listen(element, attribute.event, dispatch);
      }
      element.$$handlers[attribute.event] = attribute;
    } else {
      const held = $$matching(before, attribute);
      if (held === undefined || held.value !== attribute.value) {
        $$wrote(element, attribute);
      }
    }
  }
  for (const attribute of before) {
    if (attribute.TAG === "on") {
      if (!$$listens(after, attribute.event)) {
        element.$$handlers[attribute.event] = undefined;
      }
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
  for (const child of $$kids(vnode)) {
    element.appendChild($$element(child, dispatch, taggers));
  }
  return element;
};

const $$replaced = (before, after) =>
  before.TAG !== after.TAG ||
  ((after.TAG === "node" || after.TAG === "keyed") &&
    (before.tag !== after.tag || before.namespace !== after.namespace));

const $$keyed = (element, before, after, dispatch, taggers) => {
  const standing = Array.from(element.childNodes);
  const existing = new Map();
  before.forEach((pair, index) =>
    existing.set(pair[0], { vnode: pair[1], node: standing[index] }),
  );
  const wanted = new Set(after.map((pair) => pair[0]));
  for (const [key, held] of existing) {
    if (!wanted.has(key)) element.removeChild(held.node);
  }
  after.forEach((pair, index) => {
    const held = existing.get(pair[0]);
    const node =
      held === undefined
        ? $$element(pair[1], dispatch, taggers)
        : $$patch(element, held.node, held.vnode, pair[1], dispatch, taggers);
    const settled = element.childNodes[index];
    if (settled !== node) element.insertBefore(node, settled ?? null);
  });
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
  for (let index = shared; index < after.length; index += 1) {
    element.appendChild($$element(after[index], dispatch, taggers));
  }
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

const $$ran = (command, dispatch) => {
  if (command.TAG === "Batch") {
    for (let rest = command._0; rest !== 0; rest = rest.tl) {
      $$ran(rest.hd, dispatch);
    }
  } else if (command.TAG === "Perform") {
    command._0(dispatch);
  }
};

const $$active = new Map();

const $$gathered = (subscription, found) => {
  if (subscription.TAG === "Batch") {
    for (let rest = subscription._0; rest !== 0; rest = rest.tl) {
      $$gathered(rest.hd, found);
    }
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

const $$mount = (program, node, flags) => {
  const host = program.host(node);
  let model;
  let page;
  const dispatch = (msg) => {
    const [stepped, command] = program.step(msg, model);
    model = stepped;
    const next = program.page(model);
    $$children(host, page.body, next.body, dispatch, undefined);
    $$titled(next.title);
    page = next;
    $$ran(command, dispatch);
    $$reconciled(program.watch(model), dispatch);
  };
  const [initial, opening] = program.start(flags, dispatch);
  model = initial;
  page = program.page(model);
  host.textContent = "";
  for (const node of page.body) {
    host.appendChild($$element(node, dispatch, undefined));
  }
  $$titled(page.title);
  $$ran(opening, dispatch);
  $$reconciled(program.watch(model), dispatch);
  return { ports: program.wiring() };
};
