const $$Parser$isSubString = (small, offset, row, col, big) => {
  const length = small.length;
  let fits = offset + length <= big.length;
  for (let index = 0; fits && index < length; ) {
    const code = big.charCodeAt(offset);
    fits = small[index++] === big[offset++];
    if (!fits) break;
    if (code === 0x0a) {
      row++;
      col = 1;
    } else {
      col++;
      if ((code & 0xf800) === 0xd800) fits = small[index++] === big[offset++];
    }
  }
  return [fits ? offset : -1, row, col];
};

const $$Parser$isSubChar = (predicate, offset, text) => {
  if (text.length <= offset) return -1;
  if ((text.charCodeAt(offset) & 0xf800) === 0xd800)
    return predicate(text.substr(offset, 2)) ? offset + 2 : -1;
  if (!predicate(text[offset])) return -1;
  return text[offset] === "\n" ? -2 : offset + 1;
};

const $$Parser$isAsciiCode = (code, offset, text) => text.charCodeAt(offset) === code;

const $$Parser$chompBase10 = (offset, text) => {
  for (; offset < text.length; offset++) {
    const code = text.charCodeAt(offset);
    if (code < 0x30 || 0x39 < code) return offset;
  }
  return offset;
};

const $$Parser$consumeBase = (base, offset, text) => {
  let total = 0;
  for (; offset < text.length; offset++) {
    const digit = text.charCodeAt(offset) - 0x30;
    if (digit < 0 || base <= digit) break;
    total = base * total + digit;
  }
  return [offset, total];
};

const $$Parser$consumeBase16 = (offset, text) => {
  let total = 0;
  for (; offset < text.length; offset++) {
    const code = text.charCodeAt(offset);
    if (0x30 <= code && code <= 0x39) total = 16 * total + code - 0x30;
    else if (0x41 <= code && code <= 0x46) total = 16 * total + code - 55;
    else if (0x61 <= code && code <= 0x66) total = 16 * total + code - 87;
    else break;
  }
  return [offset, total];
};

const $$Parser$findSubString = (small, offset, row, col, big) => {
  const found = big.indexOf(small, offset);
  const target = found < 0 ? big.length : found + small.length;
  while (offset < target) {
    const code = big.charCodeAt(offset++);
    if (code === 0x0a) {
      col = 1;
      row++;
    } else {
      col++;
      if ((code & 0xf800) === 0xd800) offset++;
    }
  }
  return [found, row, col];
};

export {
  $$Parser$isSubString,
  $$Parser$isSubChar,
  $$Parser$isAsciiCode,
  $$Parser$chompBase10,
  $$Parser$consumeBase,
  $$Parser$consumeBase16,
  $$Parser$findSubString,
};
