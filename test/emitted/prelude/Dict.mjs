import * as Dartea_runtime from "./Dartea_runtime.mjs";
import * as Basics from "./Basics.mjs";
import * as List from "./List.mjs";
import * as Maybe from "./Maybe.mjs";
const Red = "Red";
const Black = "Black";
const RBNode_elm_builtin = (_0, _1, _2, _3, _4) => ({ _0: _0, _1: _1, _2: _2, _3: _3, _4: _4 });
const RBEmpty_elm_builtin = "RBEmpty_elm_builtin";
const empty = RBEmpty_elm_builtin;
const get = (targetKey, dict) => {
  while (true) {
    if (dict === "RBEmpty_elm_builtin") {
      return Maybe.Nothing;
    } else {
      const key = dict._1;
      const value = dict._2;
      const left = dict._3;
      const right = dict._4;
      const $s1 = Dartea_runtime.$$cmp(targetKey, key);
      const $s2 = ($s1 < 0) ? "LT" : ($s1 === 0) ? "EQ" : "GT";
      switch ($s2) {
        case "LT":
          const $s3 = targetKey;
          const $s4 = left;
          targetKey = $s3;
          dict = $s4;
          continue;
        case "EQ":
          return Maybe.Just(value);
        case "GT":
          const $s5 = targetKey;
          const $s6 = right;
          targetKey = $s5;
          dict = $s6;
          continue;
      }
    }
  }
};
const member = (key$1, dict$1) => {
  const $s7 = get(key$1, dict$1);
  if (typeof $s7 === "object") {
    return true;
  } else {
    return false;
  }
};
const sizeHelp = (n, dict$2) => {
  while (true) {
    if (dict$2 === "RBEmpty_elm_builtin") {
      return n;
    } else {
      const left$1 = dict$2._3;
      const right$1 = dict$2._4;
      const $s8 = sizeHelp(n + 1, right$1);
      const $s9 = left$1;
      n = $s8;
      dict$2 = $s9;
      continue;
    }
  }
};
const size = dict$3 => sizeHelp(0, dict$3);
const isEmpty = dict$4 => {
  if (dict$4 === "RBEmpty_elm_builtin") {
    return true;
  } else {
    return false;
  }
};
const balance = (color, key$2, value$1, left$2, right$2) => {
  const $dt0 = () => {
  let $s10;
  const $dt0$2 = () => RBNode_elm_builtin(color, key$2, value$1, left$2, right$2);
  if (typeof left$2 === "object") {
    if (left$2._0 === "Red") {
      if (typeof left$2._3 === "object") {
        if (left$2._3._0 === "Red") {
          const llK = left$2._3._1;
          const llV = left$2._3._2;
          const llLeft = left$2._3._3;
          const llRight = left$2._3._4;
          const lV$1 = left$2._2;
          const lK$1 = left$2._1;
          const lRight$1 = left$2._4;
          $s10 = RBNode_elm_builtin(Red, lK$1, lV$1, RBNode_elm_builtin(Black, llK, llV, llLeft, llRight), RBNode_elm_builtin(Black, key$2, value$1, lRight$1, right$2));
        } else {
          $s10 = $dt0$2();
        }
      } else {
        $s10 = $dt0$2();
      }
    } else {
      $s10 = $dt0$2();
    }
  } else {
    $s10 = $dt0$2();
  }
  return $s10;
};
  if (typeof right$2 === "object") {
    if (right$2._0 === "Red") {
      const rK = right$2._1;
      const rV = right$2._2;
      const rLeft = right$2._3;
      const rRight = right$2._4;
      const $dt0$1 = () => RBNode_elm_builtin(color, rK, rV, RBNode_elm_builtin(Red, key$2, value$1, left$2, rLeft), rRight);
      if (typeof left$2 === "object") {
        if (left$2._0 === "Red") {
          const lK = left$2._1;
          const lV = left$2._2;
          const lLeft = left$2._3;
          const lRight = left$2._4;
          return RBNode_elm_builtin(Red, key$2, value$1, RBNode_elm_builtin(Black, lK, lV, lLeft, lRight), RBNode_elm_builtin(Black, rK, rV, rLeft, rRight));
        } else {
          return $dt0$1();
        }
      } else {
        return $dt0$1();
      }
    } else {
      return $dt0();
    }
  } else {
    return $dt0();
  }
};
const insertHelp = (key$3, value$2, dict$5) => {
  if (dict$5 === "RBEmpty_elm_builtin") {
    return RBNode_elm_builtin(Red, key$3, value$2, RBEmpty_elm_builtin, RBEmpty_elm_builtin);
  } else {
    const nColor = dict$5._0;
    const nKey = dict$5._1;
    const nValue = dict$5._2;
    const nLeft = dict$5._3;
    const nRight = dict$5._4;
    const $s11 = Dartea_runtime.$$cmp(key$3, nKey);
    const $s12 = ($s11 < 0) ? "LT" : ($s11 === 0) ? "EQ" : "GT";
    switch ($s12) {
      case "LT":
        return balance(nColor, nKey, nValue, insertHelp(key$3, value$2, nLeft), nRight);
      case "EQ":
        return RBNode_elm_builtin(nColor, nKey, value$2, nLeft, nRight);
      case "GT":
        return balance(nColor, nKey, nValue, nLeft, insertHelp(key$3, value$2, nRight));
    }
  }
};
const insert = (key$4, value$3, dict$6) => {
  const $s13 = insertHelp(key$4, value$3, dict$6);
  if (typeof $s13 === "object") {
    if ($s13._0 === "Red") {
      const k = $s13._1;
      const v = $s13._2;
      const l = $s13._3;
      const r = $s13._4;
      return RBNode_elm_builtin(Black, k, v, l, r);
    } else {
      const x$1 = $s13;
      return x$1;
    }
  } else {
    const x = $s13;
    return x;
  }
};
const moveRedLeft = dict$7 => {
  const $dt0$3 = () => {
  const rLeft$2 = dict$7._4._3;
  const rK$2 = dict$7._4._1;
  const rV$2 = dict$7._4._2;
  const rClr$1 = dict$7._4._0;
  const rRight$2 = dict$7._4._4;
  const lK$3 = dict$7._3._1;
  const lV$3 = dict$7._3._2;
  const lLeft$2 = dict$7._3._3;
  const lRight$3 = dict$7._3._4;
  const k$2 = dict$7._1;
  const v$2 = dict$7._2;
  const clr$1 = dict$7._0;
  const lClr$1 = dict$7._3._0;
  let $s14;
  if (clr$1 === "Black") {
    $s14 = RBNode_elm_builtin(Black, k$2, v$2, RBNode_elm_builtin(Red, lK$3, lV$3, lLeft$2, lRight$3), RBNode_elm_builtin(Red, rK$2, rV$2, rLeft$2, rRight$2));
  } else {
    $s14 = RBNode_elm_builtin(Black, k$2, v$2, RBNode_elm_builtin(Red, lK$3, lV$3, lLeft$2, lRight$3), RBNode_elm_builtin(Red, rK$2, rV$2, rLeft$2, rRight$2));
  }
  return $s14;
};
  if (typeof dict$7 === "object") {
    if (typeof dict$7._3 === "object") {
      if (typeof dict$7._4 === "object") {
        if (typeof dict$7._4._3 === "object") {
          if (dict$7._4._3._0 === "Red") {
            const rLeft$1 = dict$7._4._3;
            const rlK = dict$7._4._3._1;
            const rlV = dict$7._4._3._2;
            const rlL = dict$7._4._3._3;
            const rlR = dict$7._4._3._4;
            const rK$1 = dict$7._4._1;
            const rV$1 = dict$7._4._2;
            const rClr = dict$7._4._0;
            const rRight$1 = dict$7._4._4;
            const lK$2 = dict$7._3._1;
            const lV$2 = dict$7._3._2;
            const lLeft$1 = dict$7._3._3;
            const lRight$2 = dict$7._3._4;
            const k$1 = dict$7._1;
            const v$1 = dict$7._2;
            const clr = dict$7._0;
            const lClr = dict$7._3._0;
            return RBNode_elm_builtin(Red, rlK, rlV, RBNode_elm_builtin(Black, k$1, v$1, RBNode_elm_builtin(Red, lK$2, lV$2, lLeft$1, lRight$2), rlL), RBNode_elm_builtin(Black, rK$1, rV$1, rlR, rRight$1));
          } else {
            return $dt0$3();
          }
        } else {
          return $dt0$3();
        }
      } else {
        return dict$7;
      }
    } else {
      return dict$7;
    }
  } else {
    return dict$7;
  }
};
const getMin = dict$8 => {
  while (true) {
    if (typeof dict$8 === "object") {
      if (typeof dict$8._3 === "object") {
        const left$3 = dict$8._3;
        const $s15 = left$3;
        dict$8 = $s15;
        continue;
      } else {
        return dict$8;
      }
    } else {
      return dict$8;
    }
  }
};
const removeMin = dict$9 => {
  if (typeof dict$9 === "object") {
    if (typeof dict$9._3 === "object") {
      const left$4 = dict$9._3;
      const lColor = dict$9._3._0;
      const lLeft$3 = dict$9._3._3;
      const key$5 = dict$9._1;
      const value$4 = dict$9._2;
      const color$1 = dict$9._0;
      const right$3 = dict$9._4;
      if (lColor === "Black") {
        const $dt0$4 = () => {
  const $s16 = moveRedLeft(dict$9);
  let $s17;
  if (typeof $s16 === "object") {
    const nColor$1 = $s16._0;
    const nKey$1 = $s16._1;
    const nValue$1 = $s16._2;
    const nLeft$1 = $s16._3;
    const nRight$1 = $s16._4;
    $s17 = balance(nColor$1, nKey$1, nValue$1, removeMin(nLeft$1), nRight$1);
  } else {
    $s17 = RBEmpty_elm_builtin;
  }
  return $s17;
};
        if (typeof lLeft$3 === "object") {
          if (lLeft$3._0 === "Red") {
            return RBNode_elm_builtin(color$1, key$5, value$4, removeMin(left$4), right$3);
          } else {
            return $dt0$4();
          }
        } else {
          return $dt0$4();
        }
      } else {
        return RBNode_elm_builtin(color$1, key$5, value$4, removeMin(left$4), right$3);
      }
    } else {
      return RBEmpty_elm_builtin;
    }
  } else {
    return RBEmpty_elm_builtin;
  }
};
const moveRedRight = dict$10 => {
  const $dt0$5 = () => {
  const lLeft$4 = dict$10._3._3;
  const rK$4 = dict$10._4._1;
  const rV$4 = dict$10._4._2;
  const rLeft$4 = dict$10._4._3;
  const rRight$4 = dict$10._4._4;
  const lK$5 = dict$10._3._1;
  const lV$5 = dict$10._3._2;
  const rClr$3 = dict$10._4._0;
  const lRight$5 = dict$10._3._4;
  const k$4 = dict$10._1;
  const v$4 = dict$10._2;
  const clr$3 = dict$10._0;
  const lClr$3 = dict$10._3._0;
  let $s18;
  if (clr$3 === "Black") {
    $s18 = RBNode_elm_builtin(Black, k$4, v$4, RBNode_elm_builtin(Red, lK$5, lV$5, lLeft$4, lRight$5), RBNode_elm_builtin(Red, rK$4, rV$4, rLeft$4, rRight$4));
  } else {
    $s18 = RBNode_elm_builtin(Black, k$4, v$4, RBNode_elm_builtin(Red, lK$5, lV$5, lLeft$4, lRight$5), RBNode_elm_builtin(Red, rK$4, rV$4, rLeft$4, rRight$4));
  }
  return $s18;
};
  if (typeof dict$10 === "object") {
    if (typeof dict$10._3 === "object") {
      if (typeof dict$10._4 === "object") {
        if (typeof dict$10._3._3 === "object") {
          if (dict$10._3._3._0 === "Red") {
            const llK$1 = dict$10._3._3._1;
            const llV$1 = dict$10._3._3._2;
            const llLeft$1 = dict$10._3._3._3;
            const llRight$1 = dict$10._3._3._4;
            const rK$3 = dict$10._4._1;
            const rV$3 = dict$10._4._2;
            const rLeft$3 = dict$10._4._3;
            const rRight$3 = dict$10._4._4;
            const lK$4 = dict$10._3._1;
            const lV$4 = dict$10._3._2;
            const rClr$2 = dict$10._4._0;
            const lRight$4 = dict$10._3._4;
            const k$3 = dict$10._1;
            const v$3 = dict$10._2;
            const clr$2 = dict$10._0;
            const lClr$2 = dict$10._3._0;
            return RBNode_elm_builtin(Red, lK$4, lV$4, RBNode_elm_builtin(Black, llK$1, llV$1, llLeft$1, llRight$1), RBNode_elm_builtin(Black, k$3, v$3, lRight$4, RBNode_elm_builtin(Red, rK$3, rV$3, rLeft$3, rRight$3)));
          } else {
            return $dt0$5();
          }
        } else {
          return $dt0$5();
        }
      } else {
        return dict$10;
      }
    } else {
      return dict$10;
    }
  } else {
    return dict$10;
  }
};
const removeHelpPrepEQGT = (targetKey$1, dict$11, color$2, key$6, value$5, left$5, right$4) => {
  const $dt0$6 = () => {
  let $s19;
  if (typeof right$4 === "object") {
    if (right$4._0 === "Black") {
      if (typeof right$4._3 === "object") {
        if (right$4._3._0 === "Black") {
          $s19 = moveRedRight(dict$11);
        } else {
          $s19 = dict$11;
        }
      } else {
        $s19 = moveRedRight(dict$11);
      }
    } else {
      $s19 = dict$11;
    }
  } else {
    $s19 = dict$11;
  }
  return $s19;
};
  if (typeof left$5 === "object") {
    if (left$5._0 === "Red") {
      const lK$6 = left$5._1;
      const lV$6 = left$5._2;
      const lLeft$5 = left$5._3;
      const lRight$6 = left$5._4;
      return RBNode_elm_builtin(color$2, lK$6, lV$6, lLeft$5, RBNode_elm_builtin(Red, key$6, value$5, lRight$6, right$4));
    } else {
      return $dt0$6();
    }
  } else {
    return $dt0$6();
  }
};
const removeHelp = (targetKey$2, dict$12) => {
  if (dict$12 === "RBEmpty_elm_builtin") {
    return RBEmpty_elm_builtin;
  } else {
    const color$3 = dict$12._0;
    const key$7 = dict$12._1;
    const value$6 = dict$12._2;
    const left$6 = dict$12._3;
    const right$5 = dict$12._4;
    if (Dartea_runtime.$$cmp(targetKey$2, key$7) < 0) {
      const $dt0$7 = () => RBNode_elm_builtin(color$3, key$7, value$6, removeHelp(targetKey$2, left$6), right$5);
      if (typeof left$6 === "object") {
        if (left$6._0 === "Black") {
          const lLeft$6 = left$6._3;
          const $dt0$8 = () => {
  const $s20 = moveRedLeft(dict$12);
  let $s21;
  if (typeof $s20 === "object") {
    const nColor$2 = $s20._0;
    const nKey$2 = $s20._1;
    const nValue$2 = $s20._2;
    const nLeft$2 = $s20._3;
    const nRight$2 = $s20._4;
    $s21 = balance(nColor$2, nKey$2, nValue$2, removeHelp(targetKey$2, nLeft$2), nRight$2);
  } else {
    $s21 = RBEmpty_elm_builtin;
  }
  return $s21;
};
          if (typeof lLeft$6 === "object") {
            if (lLeft$6._0 === "Red") {
              return RBNode_elm_builtin(color$3, key$7, value$6, removeHelp(targetKey$2, left$6), right$5);
            } else {
              return $dt0$8();
            }
          } else {
            return $dt0$8();
          }
        } else {
          return $dt0$7();
        }
      } else {
        return $dt0$7();
      }
    } else {
      return removeHelpEQGT(targetKey$2, removeHelpPrepEQGT(targetKey$2, dict$12, color$3, key$7, value$6, left$6, right$5));
    }
  }
};
const removeHelpEQGT = (targetKey$3, dict$13) => {
  if (typeof dict$13 === "object") {
    const color$4 = dict$13._0;
    const key$8 = dict$13._1;
    const value$7 = dict$13._2;
    const left$7 = dict$13._3;
    const right$6 = dict$13._4;
    if (Dartea_runtime.$$eq(targetKey$3, key$8)) {
      const $s22 = getMin(right$6);
      if (typeof $s22 === "object") {
        const minKey = $s22._1;
        const minValue = $s22._2;
        return balance(color$4, minKey, minValue, left$7, removeMin(right$6));
      } else {
        return RBEmpty_elm_builtin;
      }
    } else {
      return balance(color$4, key$8, value$7, left$7, removeHelp(targetKey$3, right$6));
    }
  } else {
    return RBEmpty_elm_builtin;
  }
};
const remove = (key$9, dict$14) => {
  const $s23 = removeHelp(key$9, dict$14);
  if (typeof $s23 === "object") {
    if ($s23._0 === "Red") {
      const k$5 = $s23._1;
      const v$5 = $s23._2;
      const l$1 = $s23._3;
      const r$1 = $s23._4;
      return RBNode_elm_builtin(Black, k$5, v$5, l$1, r$1);
    } else {
      const x$3 = $s23;
      return x$3;
    }
  } else {
    const x$2 = $s23;
    return x$2;
  }
};
const update = (targetKey$4, alter, dictionary) => {
  const $s24 = alter(get(targetKey$4, dictionary));
  if (typeof $s24 === "object") {
    const value$8 = $s24._0;
    return insert(targetKey$4, value$8, dictionary);
  } else {
    return remove(targetKey$4, dictionary);
  }
};
const singleton = (key$10, value$9) => RBNode_elm_builtin(Black, key$10, value$9, RBEmpty_elm_builtin, RBEmpty_elm_builtin);
const foldl = (func, acc, dict$15) => {
  while (true) {
    if (dict$15 === "RBEmpty_elm_builtin") {
      return acc;
    } else {
      const key$11 = dict$15._1;
      const value$10 = dict$15._2;
      const left$8 = dict$15._3;
      const right$7 = dict$15._4;
      const $s25 = func;
      const $s26 = Dartea_runtime.$$curry(func, [key$11, value$10, foldl(func, acc, left$8)]);
      const $s27 = right$7;
      func = $s25;
      acc = $s26;
      dict$15 = $s27;
      continue;
    }
  }
};
const union = (t1, t2) => foldl(insert, t2, t1);
const filter = (isGood, dict$16) => foldl((k$6, v$6, d) => {
  if (isGood(k$6, v$6)) {
    return insert(k$6, v$6, d);
  } else {
    return d;
  }
}, RBEmpty_elm_builtin, dict$16);
const intersect = (t1$1, t2$1) => filter((k$7, $p1) => member(k$7, t2$1), t1$1);
const diff = (t1$2, t2$2) => foldl((k$8, v$7, t) => remove(k$8, t), t1$2, t2$2);
const foldr = (func$1, acc$1, t$1) => {
  while (true) {
    if (t$1 === "RBEmpty_elm_builtin") {
      return acc$1;
    } else {
      const key$12 = t$1._1;
      const value$11 = t$1._2;
      const left$9 = t$1._3;
      const right$8 = t$1._4;
      const $s28 = func$1;
      const $s29 = Dartea_runtime.$$curry(func$1, [key$12, value$11, foldr(func$1, acc$1, right$8)]);
      const $s30 = left$9;
      func$1 = $s28;
      acc$1 = $s29;
      t$1 = $s30;
      continue;
    }
  }
};
const toList = dict$17 => foldr((key$13, value$12, list) => ({ hd: [key$13, value$12], tl: list }), 0, dict$17);
const merge = (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) => {
  const stepState = (rKey, rValue, $p2) => {
  while (true) {
    const list$1 = $p2[0];
    const result = $p2[1];
    if (list$1 === 0) {
      return [list$1, Dartea_runtime.$$curry(rightStep, [rKey, rValue, result])];
    } else {
      const lKey = list$1.hd[0];
      const lValue = list$1.hd[1];
      const rest = list$1.tl;
      if (Dartea_runtime.$$cmp(lKey, rKey) < 0) {
        const $s31 = rKey;
        const $s32 = rValue;
        const $s33 = [rest, Dartea_runtime.$$curry(leftStep, [lKey, lValue, result])];
        rKey = $s31;
        rValue = $s32;
        $p2 = $s33;
        continue;
      } else {
        if (Dartea_runtime.$$cmp(lKey, rKey) > 0) {
          return [list$1, Dartea_runtime.$$curry(rightStep, [rKey, rValue, result])];
        } else {
          return [rest, Dartea_runtime.$$curry(bothStep, [lKey, lValue, rValue, result])];
        }
      }
    }
  }
};
  const $s34 = foldl(stepState, [toList(leftDict), initialResult], rightDict);
  const leftovers = $s34[0];
  const intermediateResult = $s34[1];
  return List.foldl(($p0, result$1) => {
  const k$9 = $p0[0];
  const v$8 = $p0[1];
  return Dartea_runtime.$$curry(leftStep, [k$9, v$8, result$1]);
}, intermediateResult, leftovers);
};
const map = (func$2, dict$18) => {
  if (dict$18 === "RBEmpty_elm_builtin") {
    return RBEmpty_elm_builtin;
  } else {
    const color$5 = dict$18._0;
    const key$14 = dict$18._1;
    const value$13 = dict$18._2;
    const left$10 = dict$18._3;
    const right$9 = dict$18._4;
    return RBNode_elm_builtin(color$5, key$14, Dartea_runtime.$$apply2(func$2, key$14, value$13), map(func$2, left$10), map(func$2, right$9));
  }
};
const partition = (isGood$1, dict$19) => {
  const add = (key$15, value$14, $p2$1) => {
  const t1$3 = $p2$1[0];
  const t2$3 = $p2$1[1];
  if (isGood$1(key$15, value$14)) {
    return [insert(key$15, value$14, t1$3), t2$3];
  } else {
    return [t1$3, insert(key$15, value$14, t2$3)];
  }
};
  return foldl(add, [RBEmpty_elm_builtin, RBEmpty_elm_builtin], dict$19);
};
const keys = dict$20 => foldr((key$16, value$15, keyList) => ({ hd: key$16, tl: keyList }), 0, dict$20);
const values = dict$21 => foldr((key$17, value$16, valueList) => ({ hd: value$16, tl: valueList }), 0, dict$21);
const fromList = assocs => List.foldl(($p0$1, dict$22) => {
  const key$18 = $p0$1[0];
  const value$17 = $p0$1[1];
  return insert(key$18, value$17, dict$22);
}, RBEmpty_elm_builtin, assocs);
export { diff, empty, filter, foldl, foldr, fromList, get, insert, intersect, isEmpty, keys, map, member, merge, partition, remove, singleton, size, toList, union, update, values };
