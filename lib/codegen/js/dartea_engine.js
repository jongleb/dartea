const $$element = (vnode, dispatch) => {
  if (vnode.TAG === "text") return document.createTextNode(vnode.text);
  const element = document.createElement(vnode.tag);
  for (const attribute of vnode.attributes) {
    if (attribute.TAG === "attribute") {
      element.setAttribute(attribute.key, attribute.value);
    } else if (attribute.TAG === "style") {
      element.style.setProperty(attribute.key, attribute.value);
    } else {
      element.addEventListener(attribute.event, () => dispatch(attribute.msg));
    }
  }
  for (const child of vnode.children) {
    element.appendChild($$element(child, dispatch));
  }
  return element;
};

const $$mount = (program, host) => {
  const { init, update, view } = program.config;
  let model = init;
  const draw = () => {
    host.textContent = "";
    host.appendChild($$element(view(model), dispatch));
  };
  const dispatch = (msg) => {
    model = update(msg, model);
    draw();
  };
  draw();
};
