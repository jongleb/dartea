const Red = "Red";
const Green = "Green";
const Blue = "Blue";
const toCode = color => {
  switch (color) {
    case "Red":
      return 1;
    case "Green":
      return 2;
    case "Blue":
      return 3;
  }
};
const name = color$1 => {
  switch (color$1) {
    case "Red":
      return "red";
    case "Green":
      return "green";
    case "Blue":
      return "blue";
  }
};
export { Blue, Green, Red, name, toCode };
