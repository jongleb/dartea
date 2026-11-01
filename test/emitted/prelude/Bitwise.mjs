const and = (a, b) => a & b;
const or = (a, b) => a | b;
const xor = (a, b) => a ^ b;
const complement = x => ~x;
const shiftLeftBy = (a, b) => b << a;
const shiftRightBy = (a, b) => b >> a;
const shiftRightZfBy = (a, b) => b >>> a;
export { and, complement, or, shiftLeftBy, shiftRightBy, shiftRightZfBy, xor };
