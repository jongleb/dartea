import * as Color from "./Color.mjs";
import * as Palette from "./Palette.mjs";
const label = n => Color.name(Palette.slot(n));
const main = Palette.distance(Palette.slot(0), Palette.brighter(Palette.slot(1))) + Color.toCode(Palette.slot(2));
export { label, main };
