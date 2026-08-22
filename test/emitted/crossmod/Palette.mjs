import * as Color from "./Color.mjs";
const slot = n => {
  if (n === 0) {
    return Color.Red;
  } else {
    if (n === 1) {
      return Color.Green;
    } else {
      return Color.Blue;
    }
  }
};
const brighter = color => {
  switch (color) {
    case "Red":
      return Color.Green;
    case "Green":
      return Color.Blue;
    case "Blue":
      return Color.Red;
  }
};
const distance = (from, to) => Color.toCode(to) - Color.toCode(from);
export { brighter, distance, slot };
