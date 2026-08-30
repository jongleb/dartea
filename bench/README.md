# bench

dartea for js-framework-benchmark. The layout is what it expects in `frameworks/keyed/dartea`: `package.json`, `index.html`, `src/Main.elm`.

```
sh bench/run.sh
```

The script clones js-framework-benchmark into `_bench`, copies this folder in, compiles `Main.elm` with the dartea from `_build`, builds the official vanillajs, elm, react-hooks, svelte and solid, then runs the official runner headless and builds the official result page at `_bench/webdriver-ts-results/dist/index.html`. `benchmark.png` is a crop of that page from the last run.

How the runner measures. It serves every implementation from a local server and opens its `index.html` in Chrome through the DevTools protocol. The page layout is fixed by the spec, that is how it knows where to click. Buttons have the ids `run`, `runlots`, `add`, `update`, `clear`, `swaprows`. A row is a `tr` where the first `a` selects and the `a` with a `span.glyphicon` inside removes. For each measurement it opens a fresh tab, does the warmup clicks, starts a Chrome trace, clicks once and reads the time from the click event to the end of the last paint out of the trace. 15 iterations per benchmark, the table shows the median. Memory comes from the DevTools heap numbers, sizes from the built files.

The Elm program is the official `frameworks/keyed/elm` one with `Array` and `Random` replaced by `List` and a small generator, because dartea does not have those modules yet.
