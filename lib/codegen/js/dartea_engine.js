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

const $$listen = (element, event, dispatch) => {
  element.addEventListener(event, (happened) => {
    const handler = element.$$handlers[event];
    if (handler === undefined) return;
    const decided = $$decided(handler, happened);
    if (decided === undefined) return;
    if (decided.stop) happened.stopPropagation();
    if (decided.prevent) happened.preventDefault();
    dispatch(decided.message);
  });
};

const $$listens = (attributes, event) =>
  attributes.some(
    (attribute) => attribute.TAG === "on" && attribute.event === event,
  );

const $$attributes = (element, before, after, dispatch) => {
  element.$$handlers = element.$$handlers ?? {};
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
      element.$$handlers[attribute.event] = attribute.handler;
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

const $$element = (vnode, dispatch) => {
  if (vnode.TAG === "text") return document.createTextNode(vnode.text);
  const element = document.createElement(vnode.tag);
  $$attributes(element, [], vnode.attributes, dispatch);
  for (const child of vnode.children) {
    element.appendChild($$element(child, dispatch));
  }
  return element;
};

const $$replaced = (before, after) =>
  before.TAG !== after.TAG || (after.TAG === "node" && before.tag !== after.tag);

const $$patch = (parent, index, before, after, dispatch) => {
  const element = parent.childNodes[index];
  if ($$replaced(before, after)) {
    parent.replaceChild($$element(after, dispatch), element);
    return;
  }
  if (after.TAG === "text") {
    if (before.text !== after.text) element.nodeValue = after.text;
    return;
  }
  $$attributes(element, before.attributes, after.attributes, dispatch);
  const shared = Math.min(before.children.length, after.children.length);
  for (let index = 0; index < shared; index += 1) {
    $$patch(element, index, before.children[index], after.children[index], dispatch);
  }
  for (let index = shared; index < after.children.length; index += 1) {
    element.appendChild($$element(after.children[index], dispatch));
  }
  for (let index = before.children.length - 1; index >= shared; index -= 1) {
    element.removeChild(element.childNodes[index]);
  }
};

const $$mount = (program, host) => {
  const { init, update, view } = program.config;
  let model = init;
  let tree = view(model);
  const dispatch = (msg) => {
    model = update(msg, model);
    const next = view(model);
    $$patch(host, 0, tree, next, dispatch);
    tree = next;
  };
  host.textContent = "";
  host.appendChild($$element(tree, dispatch));
};
