const $$toArray = (list) => {
  const items = [];
  for (let rest = list; rest !== 0; rest = rest.tl) items.push(rest.hd);
  return items;
};

const $$VirtualDom$text = (text) => ({ TAG: "text", text });

const $$VirtualDom$node = (tag, attributes, children) => ({
  TAG: "node",
  tag,
  attributes: $$toArray(attributes),
  children: $$toArray(children),
});

const $$VirtualDom$attribute = (key, value) => ({ TAG: "attribute", key, value });
const $$VirtualDom$style = (key, value) => ({ TAG: "style", key, value });

export {
  $$VirtualDom$text,
  $$VirtualDom$node,
  $$VirtualDom$attribute,
  $$VirtualDom$style,
};
