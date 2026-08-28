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

const $$attributes = (element, before, after, dispatch, taggers) => {
  element.$$handlers = element.$$handlers ?? {};
  element.$$taggers = taggers;
  for (const attribute of after) {
    if (attribute.TAG === "attribute") {
      element.setAttribute(attribute.key, attribute.value);
    } else if (attribute.TAG === "property") {
      element[attribute.key] = attribute.value;
    } else if (attribute.TAG === "style") {
      element.style.setProperty(attribute.key, attribute.value);
    } else {
      if (element.$$handlers[attribute.event] === undefined) {
        $$listen(element, attribute.event, dispatch);
      }
      element.$$handlers[attribute.event] = attribute;
    }
  }
  for (const attribute of before) {
    if (attribute.TAG === "attribute") {
      if (!after.some((kept) => kept.TAG === "attribute" && kept.key === attribute.key)) {
        element.removeAttribute(attribute.key);
      }
    } else if (attribute.TAG === "property") {
      if (!after.some((kept) => kept.TAG === "property" && kept.key === attribute.key)) {
        element[attribute.key] = undefined;
      }
    } else if (attribute.TAG === "style") {
      if (!after.some((kept) => kept.TAG === "style" && kept.key === attribute.key)) {
        element.style.removeProperty(attribute.key);
      }
    } else if (!$$listens(after, attribute.event)) {
      element.$$handlers[attribute.event] = undefined;
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
  if (vnode.TAG === "text") return document.createTextNode(vnode.text);
  const element = document.createElement(vnode.tag);
  $$attributes(element, [], vnode.attributes, dispatch, taggers);
  for (const child of $$kids(vnode)) {
    element.appendChild($$element(child, dispatch, taggers));
  }
  return element;
};

const $$replaced = (before, after) =>
  before.TAG !== after.TAG ||
  ((after.TAG === "node" || after.TAG === "keyed") && before.tag !== after.tag);

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
  if (after.TAG === "text") {
    if (before.text !== after.text) element.nodeValue = after.text;
    return element;
  }
  $$attributes(element, before.attributes, after.attributes, dispatch, taggers);
  if (after.TAG === "keyed") {
    $$keyed(element, before.children, after.children, dispatch, taggers);
    return element;
  }
  const shared = Math.min(before.children.length, after.children.length);
  for (let index = 0; index < shared; index += 1) {
    $$patch(
      element,
      element.childNodes[index],
      before.children[index],
      after.children[index],
      dispatch,
      taggers,
    );
  }
  for (let index = shared; index < after.children.length; index += 1) {
    element.appendChild($$element(after.children[index], dispatch, taggers));
  }
  for (let index = before.children.length - 1; index >= shared; index -= 1) {
    element.removeChild(element.childNodes[index]);
  }
  return element;
};

const $$mount = (program, host) => {
  const { init, update, view } = program.config;
  let model = init;
  let tree = view(model);
  const dispatch = (msg) => {
    model = update(msg, model);
    const next = view(model);
    $$patch(host, host.childNodes[0], tree, next, dispatch, undefined);
    tree = next;
  };
  host.textContent = "";
  host.appendChild($$element(tree, dispatch, undefined));
};
