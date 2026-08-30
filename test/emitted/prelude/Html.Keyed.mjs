import * as VirtualDom from "./VirtualDom.mjs";
const node = (eta1, eta2, eta3) => VirtualDom.keyedNode(eta1, eta2, eta3);
const ol = (eta1$1, eta2$1) => VirtualDom.keyedNode("ol", eta1$1, eta2$1);
const ul = (eta1$2, eta2$2) => VirtualDom.keyedNode("ul", eta1$2, eta2$2);
export { node, ol, ul };
