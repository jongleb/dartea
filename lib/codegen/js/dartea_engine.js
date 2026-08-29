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

const $$performed = (task) => {
  const found = document.getElementById(task.id);
  if (found === null || found === undefined) {
    return { TAG: "Err", _0: { TAG: "NotFound", _0: task.id } };
  }
  found.focus();
  return { TAG: "Ok", _0: null };
};

const $$ran = (command, dispatch) => {
  if (command.TAG === "Batch") {
    for (let rest = command._0; rest !== 0; rest = rest.tl) {
      $$ran(rest.hd, dispatch);
    }
  } else if (command.TAG === "Perform") {
    const effect = command._0;
    dispatch(effect.toMsg($$performed(effect.task)));
  }
};

const $$mount = (program, host, flags) => {
  let [model, opening] = program.start(flags);
  let page = program.page(model);
  const dispatch = (msg) => {
    const [stepped, command] = program.step(msg, model);
    model = stepped;
    const next = program.page(model);
    $$children(host, page.body, next.body, dispatch, undefined);
    $$titled(next.title);
    page = next;
    $$ran(command, dispatch);
  };
  host.textContent = "";
  for (const node of page.body) {
    host.appendChild($$element(node, dispatch, undefined));
  }
  $$titled(page.title);
  $$ran(opening, dispatch);
};
