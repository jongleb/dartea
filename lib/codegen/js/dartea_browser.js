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

const $$ports = { outgoing: new Map(), incoming: new Map() };

const $$subscribers = (name) => {
  const found = $$ports.outgoing.get(name);
  if (found !== undefined) return found;
  const made = [];
  $$ports.outgoing.set(name, made);
  return made;
};

const $$Port$outgoing = (name, value) => ({
  TAG: "Perform",
  _0: () => {
    for (const send of [...$$subscribers(name)]) send(value);
  },
});

const $$Port$incoming = (name, tagger) => ({
  TAG: "Listen",
  _0: "port:" + name,
  _1: (dispatch) => {
    $$ports.incoming.set(name, (value) => dispatch(tagger(value)));
    return () => $$ports.incoming.delete(name);
  },
});

const $$Port$named = (name) => ({
  subscribe: (listener) => $$subscribers(name).push(listener),
  unsubscribe: (listener) => {
    const listeners = $$subscribers(name);
    const at = listeners.indexOf(listener);
    if (at >= 0) listeners.splice(at, 1);
  },
  send: (value) => {
    const deliver = $$ports.incoming.get(name);
    if (deliver !== undefined) deliver(value);
  },
});

const $$Port$wiring = () =>
  new Proxy({}, { get: (_holder, name) => $$Port$named(name) });

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
  wiring: $$Port$wiring,
  host: (node) => node,
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
  wiring: $$Port$wiring,
  host: () => document.body,
});

const $$Browser$element = (config) => ({
  TAG: "program",
  start: (flags) => config.init(flags),
  step: (msg, model) => config.update(msg, model),
  page: (model) => ({ title: undefined, body: [config.view(model)] }),
  watch: (model) => config.subscriptions(model),
  wiring: $$Port$wiring,
  host: (node) => node,
});

const $$currentUrl = (parse) => {
  const found = parse(location.href);
  if (found === "Nothing") throw new Error("Trying to parse an invalid URL: " + location.href);
  return found._0;
};

const $$sameOrigin = (one, other) =>
  one.protocol === other.protocol &&
  one.host === other.host &&
  JSON.stringify(one.port_) === JSON.stringify(other.port_);

const $$anchorOf = (target) => {
  for (let node = target; node !== null && node !== undefined; node = node.parentNode) {
    if (node.tagName === "A") return node;
  }
  return undefined;
};

const $$interceptLinks = (parse, dispatch, onUrlRequest) => {
  document.addEventListener("click", (event) => {
    if (event.ctrlKey || event.metaKey || event.shiftKey || event.button >= 1) return;
    const anchor = $$anchorOf(event.target);
    if (anchor === undefined || anchor.target || anchor.hasAttribute("download")) return;
    const href = anchor.href;
    if (typeof href !== "string" || href === "") return;
    event.preventDefault();
    const next = parse(href);
    const request =
      next !== "Nothing" && $$sameOrigin($$currentUrl(parse), next._0)
        ? { TAG: "Internal", _0: next._0 }
        : { TAG: "External", _0: href };
    dispatch(onUrlRequest(request));
  });
};

const $$Browser$application = (parse, config) => ({
  TAG: "program",
  start: (flags, dispatch) => {
    const key = { TAG: "Key", notify: () => dispatch(config.onUrlChange($$currentUrl(parse))) };
    window.addEventListener("popstate", key.notify);
    $$interceptLinks(parse, dispatch, config.onUrlRequest);
    return config.init(flags, $$currentUrl(parse), key);
  },
  step: (msg, model) => config.update(msg, model),
  page: (model) => {
    const shown = config.view(model);
    return { title: shown.title, body: $$toArray(shown.body) };
  },
  watch: (model) => config.subscriptions(model),
  wiring: $$Port$wiring,
  host: () => document.body,
});

const $$Browser$pushUrl = (key, url) => ({
  TAG: "Perform",
  _0: () => {
    history.pushState({}, "", url);
    key.notify();
  },
});

const $$Browser$replaceUrl = (key, url) => ({
  TAG: "Perform",
  _0: () => {
    history.replaceState({}, "", url);
    key.notify();
  },
});

const $$Browser$go = (key, steps) => ({
  TAG: "Perform",
  _0: () => {
    if (steps !== 0) history.go(steps);
    key.notify();
  },
});

const $$Browser$load = (url) => ({
  TAG: "Perform",
  _0: () => {
    try {
      window.location = url;
    } catch {
      document.location.reload();
    }
  },
});

const $$Browser$reload = (skipCache) => ({
  TAG: "Perform",
  _0: () => document.location.reload(skipCache),
});

const $$eventTarget = (name) => (name === "window" ? window : document);

const $$Browser$on = (node, event, decoder) => ({
  TAG: "Listen",
  _0: "Browser.on:" + node + ":" + event,
  _1: (dispatch) => {
    const target = $$eventTarget(node);
    const listener = (happened) => {
      const decoded = decoder._0(happened);
      if (decoded.TAG === "Ok") dispatch(decoded._0);
    };
    target.addEventListener(event, listener);
    return () => target.removeEventListener(event, listener);
  },
});

const $$frames = (key, step) => ({
  TAG: "Listen",
  _0: key,
  _1: (dispatch) => {
    let handle = 0;
    let previous = performance.now();
    const tick = (now) => {
      handle = requestAnimationFrame(tick);
      dispatch(step(now, previous));
      previous = now;
    };
    handle = requestAnimationFrame(tick);
    return () => cancelAnimationFrame(handle);
  },
});

const $$Browser$onAnimationFrame = (toMsg) =>
  $$frames("Browser.onAnimationFrame", () => toMsg(Date.now()));

const $$Browser$onAnimationFrameDelta = (toMsg) =>
  $$frames("Browser.onAnimationFrameDelta", (now, previous) => toMsg(now - previous));

const $$Url$percentEncode = (text) => encodeURIComponent(text);

const $$Url$percentDecode = (text) => {
  try {
    return { _0: decodeURIComponent(text) };
  } catch {
    return "Nothing";
  }
};

export {
  $$Browser$sandbox,
  $$Browser$document,
  $$Browser$element,
  $$Browser$application,
  $$Browser$pushUrl,
  $$Browser$replaceUrl,
  $$Browser$go,
  $$Browser$load,
  $$Browser$reload,
  $$Browser$on,
  $$Browser$onAnimationFrame,
  $$Browser$onAnimationFrameDelta,
  $$Url$percentEncode,
  $$Url$percentDecode,
  $$Task$succeed,
  $$Task$fail,
  $$Task$andThen,
  $$Task$onError,
  $$Task$attempt,
  $$Task$perform,
  $$Dom$focus,
  $$Time$every,
  $$Port$outgoing,
  $$Port$incoming,
  $$Port$wiring,
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
