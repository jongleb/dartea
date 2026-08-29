const $$toArray = (list) => {
  const items = [];
  for (let rest = list; rest !== 0; rest = rest.tl) items.push(rest.hd);
  return items;
};

const $$VirtualDom$text = (text) => ({ TAG: "text", text });

const $$made = (TAG, namespace, tag, attributes, children) => ({
  TAG,
  tag,
  namespace,
  attributes: $$toArray(attributes),
  children: $$toArray(children),
});

const $$VirtualDom$node = (tag, attributes, children) =>
  $$made("node", undefined, tag, attributes, children);

const $$VirtualDom$nodeNS = (namespace, tag, attributes, children) =>
  $$made("node", namespace, tag, attributes, children);

const $$VirtualDom$keyedNode = (tag, attributes, children) =>
  $$made("keyed", undefined, tag, attributes, children);

const $$VirtualDom$keyedNodeNS = (namespace, tag, attributes, children) =>
  $$made("keyed", namespace, tag, attributes, children);

const $$VirtualDom$attribute = (key, value) => ({
  TAG: "attribute",
  key,
  value,
  namespace: undefined,
});

const $$VirtualDom$attributeNS = (namespace, key, value) => ({
  TAG: "attribute",
  key,
  value,
  namespace,
});
const $$VirtualDom$property = (key, value) => ({ TAG: "property", key, value });
const $$VirtualDom$style = (key, value) => ({ TAG: "style", key, value });
const $$VirtualDom$on = (event, handler) => ({ TAG: "on", event, handler });

const $$VirtualDom$map = (tagger, node) => ({ TAG: "map", tagger, node });

const $$VirtualDom$mapAttribute = (tagger, attribute) => {
  if (attribute.TAG !== "on") return attribute;
  const inside = attribute.tagger;
  return {
    TAG: "on",
    event: attribute.event,
    handler: attribute.handler,
    tagger:
      inside === undefined
        ? tagger
        : (message) => tagger(inside(message)),
  };
};

const $$lazily = (refs, thunk) => ({ TAG: "lazy", refs, thunk, node: undefined });

const $$VirtualDom$lazy = (view, x1) =>
  $$lazily([view, x1], () => view(x1));

const $$VirtualDom$lazy2 = (view, x1, x2) =>
  $$lazily([view, x1, x2], () => view(x1, x2));

const $$VirtualDom$lazy3 = (view, x1, x2, x3) =>
  $$lazily([view, x1, x2, x3], () => view(x1, x2, x3));

const $$VirtualDom$lazy4 = (view, x1, x2, x3, x4) =>
  $$lazily([view, x1, x2, x3, x4], () => view(x1, x2, x3, x4));

const $$VirtualDom$lazy5 = (view, x1, x2, x3, x4, x5) =>
  $$lazily([view, x1, x2, x3, x4, x5], () => view(x1, x2, x3, x4, x5));

const $$VirtualDom$lazy6 = (view, x1, x2, x3, x4, x5, x6) =>
  $$lazily([view, x1, x2, x3, x4, x5, x6], () => view(x1, x2, x3, x4, x5, x6));

const $$VirtualDom$lazy7 = (view, x1, x2, x3, x4, x5, x6, x7) =>
  $$lazily([view, x1, x2, x3, x4, x5, x6, x7], () => view(x1, x2, x3, x4, x5, x6, x7));

const $$VirtualDom$lazy8 = (view, x1, x2, x3, x4, x5, x6, x7, x8) =>
  $$lazily([view, x1, x2, x3, x4, x5, x6, x7, x8], () => view(x1, x2, x3, x4, x5, x6, x7, x8));

const $$Task$succeed = (value) => ({ TAG: "succeed", value });
const $$Task$fail = (error) => ({ TAG: "fail", error });

const $$binding = (callback) => ({ TAG: "binding", callback, kill: null });

const $$Task$andThen = (callback, task) => ({ TAG: "andThen", callback, task });
const $$Task$onError = (callback, task) => ({ TAG: "onError", callback, task });

const $$pending = { queue: [], working: false, spawned: 0 };

const $$enqueue = (process) => {
  $$pending.queue.push(process);
  if ($$pending.working) return;
  $$pending.working = true;
  while ($$pending.queue.length > 0) $$step($$pending.queue.shift());
  $$pending.working = false;
};

const $$step = (process) => {
  while (process.root !== null) {
    const root = process.root;
    if (root.TAG === "succeed" || root.TAG === "fail") {
      while (process.stack !== null && process.stack.wanted !== root.TAG) {
        process.stack = process.stack.rest;
      }
      if (process.stack === null) {
        process.root = null;
        process.settle(root);
        return;
      }
      process.root = process.stack.callback(
        root.TAG === "succeed" ? root.value : root.error,
      );
      process.stack = process.stack.rest;
    } else if (root.TAG === "binding") {
      root.kill = root.callback((next) => {
        process.root = next;
        $$enqueue(process);
      });
      return;
    } else {
      process.stack = {
        wanted: root.TAG === "andThen" ? "succeed" : "fail",
        callback: root.callback,
        rest: process.stack,
      };
      process.root = root.task;
    }
  }
};

const $$run = (task, settle) => {
  $$enqueue({
    id: $$pending.spawned++,
    root: task,
    stack: null,
    mailbox: [],
    settle,
  });
};

const $$Task$attempt = (toMsg, task) => ({
  TAG: "Perform",
  _0: (dispatch) =>
    $$run(task, (final) =>
      dispatch(
        toMsg(
          final.TAG === "succeed"
            ? { TAG: "Ok", _0: final.value }
            : { TAG: "Err", _0: final.error },
        ),
      ),
    ),
});

const $$Task$perform = (toMsg, task) => ({
  TAG: "Perform",
  _0: (dispatch) =>
    $$run(task, (final) => {
      if (final.TAG === "succeed") dispatch(toMsg(final.value));
    }),
});

const $$Time$every = (interval, tagger) => ({
  TAG: "Listen",
  _0: "Time.every:" + interval,
  _1: (dispatch) => {
    const id = setInterval(() => dispatch(tagger(Date.now())), interval);
    return () => clearInterval(id);
  },
});

const $$Dom$focus = (id) =>
  $$binding((deliver) => {
    const found = document.getElementById(id);
    if (found === null || found === undefined) {
      deliver($$Task$fail({ TAG: "NotFound", _0: id }));
    } else {
      found.focus();
      deliver($$Task$succeed(null));
    }
    return null;
  });

const $$Browser$sandbox = (config) => ({
  TAG: "program",
  start: () => [config.init, "None"],
  step: (msg, model) => [config.update(msg, model), "None"],
  page: (model) => ({ title: undefined, body: [config.view(model)] }),
  watch: () => "None",
});

const $$Browser$document = (config) => ({
  TAG: "program",
  start: (flags) => config.init(flags),
  step: (msg, model) => config.update(msg, model),
  page: (model) => {
    const shown = config.view(model);
    return { title: shown.title, body: $$toArray(shown.body) };
  },
  watch: (model) => config.subscriptions(model),
});

export {
  $$Browser$sandbox,
  $$Browser$document,
  $$Task$succeed,
  $$Task$fail,
  $$Task$andThen,
  $$Task$onError,
  $$Task$attempt,
  $$Task$perform,
  $$Dom$focus,
  $$Time$every,
  $$VirtualDom$on,
  $$VirtualDom$map,
  $$VirtualDom$mapAttribute,
  $$VirtualDom$lazy,
  $$VirtualDom$lazy2,
  $$VirtualDom$lazy3,
  $$VirtualDom$lazy4,
  $$VirtualDom$lazy5,
  $$VirtualDom$lazy6,
  $$VirtualDom$lazy7,
  $$VirtualDom$lazy8,
  $$VirtualDom$text,
  $$VirtualDom$property,
  $$VirtualDom$node,
  $$VirtualDom$keyedNode,
  $$VirtualDom$nodeNS,
  $$VirtualDom$keyedNodeNS,
  $$VirtualDom$attributeNS,
  $$VirtualDom$attribute,
  $$VirtualDom$style,
};
