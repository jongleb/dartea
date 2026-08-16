let curry =
  "const $$curry = (f, args) => {\n\
  \  const n = f.length === 0 ? 1 : f.length;\n\
  \  if (args.length === n) return f(...args);\n\
  \  if (args.length < n) return (...more) => $$curry(f, [...args, ...more]);\n\
  \  return $$curry(f(...args.slice(0, n)), args.slice(n));\n\
   };\n"
