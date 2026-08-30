const $$items = function* (list) {
  for (let rest = list; rest !== 0; rest = rest.tl) yield rest.hd;
};

const $$toArray = (list) => [...$$items(list)];

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

const $$Task$succeed = (value) => async () => value;

const $$Task$fail = (error) => async () => {
  throw error;
};

const $$Task$andThen = (callback, task) => async (signal) => callback(await task(signal))(signal);

const $$Task$onError = (callback, task) => async (signal) => {
  try {
    return await task(signal);
  } catch (error) {
    return callback(error)(signal);
  }
};

const $$Task$attempt = (toMsg, task) => ({
  TAG: "Perform",
  _0: async (dispatch) => {
    const { signal } = new AbortController();
    try {
      dispatch(toMsg({ TAG: "Ok", _0: await task(signal) }));
    } catch (error) {
      dispatch(toMsg({ TAG: "Err", _0: error }));
    }
  },
});

const $$Task$perform = (toMsg, task) => ({
  TAG: "Perform",
  _0: async (dispatch) => dispatch(toMsg(await task(new AbortController().signal))),
});

const $$ports = { outgoing: new Map(), incoming: new Map() };

const $$subscribers = (name) => {
  const found = $$ports.outgoing.get(name) ?? new Set();
  $$ports.outgoing.set(name, found);
  return found;
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

const $$Port$at = (name) => ({
  subscribe: (listener) => $$subscribers(name).add(listener),
  unsubscribe: (listener) => $$subscribers(name).delete(listener),
  send: (value) => $$ports.incoming.get(name)?.(value),
});

const $$Port$wiring = () =>
  new Proxy({}, { get: (_holder, name) => $$Port$at(name) });

const $$Time$every = (interval, tagger) => ({
  TAG: "Listen",
  _0: "Time.every:" + interval,
  _1: (dispatch) => {
    const id = setInterval(() => dispatch(tagger(Date.now())), interval);
    return () => clearInterval(id);
  },
});

const $$Dom$focus = (id) => async () => {
  const found = document.getElementById(id);
  if (!found) throw { TAG: "NotFound", _0: id };
  found.focus();
  return null;
};

const $$Http$requests = new Map();
const $$Http$watchers = new Map();

const $$Http$headerList = (headers) => {
  let made = 0;
  for (const [name, value] of [...headers].toReversed()) made = { hd: [name, value], tl: made };
  return made;
};

const $$Http$notify = (tracker, progress) => {
  for (const send of $$Http$watchers.get(tracker) ?? []) send(progress);
};

const $$Http$receive = async (response, tracker) => {
  if (tracker === undefined || response.body === null) return response.text();
  const size = response.headers.get("content-length");
  const total = size === null ? "Nothing" : { _0: Number(size) };
  const chunks = [];
  let received = 0;
  for await (const chunk of response.body) {
    chunks.push(chunk);
    received += chunk.length;
    $$Http$notify(tracker, { TAG: "Receiving", _0: { received, size: total } });
  }
  return new Blob(chunks).text();
};

const $$Http$raw = (kind, url, response, body) => ({
  kind,
  url: response?.url || url,
  statusCode: response?.status ?? 0,
  statusText: response?.statusText ?? "",
  headers: response === undefined ? 0 : $$Http$headerList(response.headers),
  body,
});

const $$Http$signal = (request, controller, outer) =>
  AbortSignal.any([
    controller.signal,
    ...(outer === undefined ? [] : [outer]),
    ...(request.timeout === "Nothing" ? [] : [AbortSignal.timeout(request.timeout._0)]),
  ]);

const $$Http$options = (request, controller, outer) => {
  const headers = new Headers();
  for (const held of $$items(request.headers)) headers.append(held._0, held._1);
  if (request.body !== null && request.body.mime !== "") headers.set("Content-Type", request.body.mime);
  return {
    method: request.method,
    headers,
    body: request.body?.content,
    credentials: request.allowCookiesFromOtherDomains ? "include" : "same-origin",
    signal: $$Http$signal(request, controller, outer),
  };
};

const $$Http$kind = (problem, controller) => {
  if (problem?.name === "TimeoutError") return "timeout";
  if (controller.signal.aborted || problem?.name === "AbortError") return "cancelled";
  if (problem instanceof TypeError && String(problem.message).includes("URL")) return "badUrl";
  return "network";
};

const $$Http$send = async (request, outer) => {
  const controller = new AbortController();
  const tracker = request.tracker === "Nothing" ? undefined : request.tracker._0;
  if (tracker !== undefined) $$Http$requests.set(tracker, controller);
  try {
    const response = await fetch(request.url, $$Http$options(request, controller, outer));
    if (tracker !== undefined) {
      const sent = request.body === null ? 0 : String(request.body.content).length;
      $$Http$notify(tracker, { TAG: "Sending", _0: { sent, size: sent } });
    }
    const body = await $$Http$receive(response, tracker);
    return $$Http$raw(response.ok ? "good" : "bad", request.url, response, body);
  } catch (problem) {
    return $$Http$raw($$Http$kind(problem, controller), request.url, undefined, "");
  } finally {
    if (tracker !== undefined) $$Http$requests.delete(tracker);
  }
};

const $$Http$emptyBody = null;
const $$Http$pair = (mime, content) => ({ mime, content });

const $$Http$toFormData = (parts) => {
  const data = new FormData();
  for (const part of $$items(parts)) data.append(part.mime, part.content);
  return data;
};

const $$Http$expect = (toMsg) => toMsg;

const $$Http$request = (request) => ({
  TAG: "Perform",
  _0: async (dispatch) => {
    const raw = await $$Http$send(request);
    if (raw.kind !== "cancelled") dispatch(request.expect(raw));
  },
});

const $$Http$cancel = (tracker) => ({
  TAG: "Perform",
  _0: () => {
    $$Http$requests.get(tracker)?.abort();
    $$Http$requests.delete(tracker);
  },
});

const $$Http$track = (tracker, toMsg) => ({
  TAG: "Listen",
  _0: "Http.track:" + tracker,
  _1: (dispatch) => {
    const send = (progress) => dispatch(toMsg(progress));
    const found = $$Http$watchers.get(tracker) ?? new Set();
    found.add(send);
    $$Http$watchers.set(tracker, found);
    return () => found.delete(send);
  },
});

const $$Http$toTask = (request) => async (signal) => {
  const outcome = request.expect(await $$Http$send(request, signal));
  if (outcome.TAG === "Ok") return outcome._0;
  throw outcome._0;
};

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

const $$interceptLinks = (parse, dispatch, onUrlRequest) => {
  document.addEventListener("click", (event) => {
    if (event.ctrlKey || event.metaKey || event.shiftKey || event.button >= 1) return;
    const anchor = event.target.closest?.("a");
    if (!anchor || anchor.target || anchor.hasAttribute("download")) return;
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
  _0: () => {
    document.location.reload(skipCache);
  },
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
  $$Http$emptyBody,
  $$Http$pair,
  $$Http$toFormData,
  $$Http$expect,
  $$Http$request,
  $$Http$cancel,
  $$Http$track,
  $$Http$toTask,
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
