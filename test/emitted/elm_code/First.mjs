const E = _0 => ({ TAG: "E", _0: _0 });
const F = _0 => ({ TAG: "F", _0: _0 });
const C = "C";
const D = "D";
const testAgain = x => {
  switch (x.TAG) {
    case "E":
      switch (x._0) {
        case "C":
          return 1;
        case "D":
          return 3;
      }
    case "F":
      if (x._0 === "x") {
        return 4;
      } else {
        return 5;
      }
  }
};
const result = testAgain(F("lol"));
export { C, D, E, F, result, testAgain };
