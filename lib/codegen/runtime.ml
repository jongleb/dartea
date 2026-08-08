let core =
  "const $$matchError = (value) => {\n\
  \  console.error(\"Pattern match failed for value:\", value);\n\
  \  return null;\n\
   };\n\
   const $$curry = (f, args) => {\n\
  \  const n = f.length === 0 ? 1 : f.length;\n\
  \  if (args.length === n) return f(...args);\n\
  \  if (args.length < n) return (...more) => $$curry(f, [...args, ...more]);\n\
  \  return $$curry(f(...args.slice(0, n)), args.slice(n));\n\
   };\n"

let builtins =
  [
    ("identity", "const identity = (x) => x;");
    ("always", "const always = (a, b) => a;");
    ("fromInt", "const fromInt = (n) => String(n);");
    ( "toInt",
      "const toInt = (s) => s !== \"\" && Number.isInteger(Number(s)) ? { _0: \
       Number(s) } : \"Nothing\";" );
    ("length", "const length = (s) => s.length;");
    ("append", "const append = (a, b) => a + b;");
    ("pair", "const pair = (a, b) => [a, b];");
    ("first", "const first = (t) => t[0];");
    ("second", "const second = (t) => t[1];");
  ]

let reserved = "$$matchError" :: "$$curry" :: "console" :: List.map fst builtins
