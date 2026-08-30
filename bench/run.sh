#!/bin/sh
set -e
here="$(cd "$(dirname "$0")" && pwd)"
work="${JFB:-$here/../_bench}"
frameworks="${FRAMEWORKS:-vanillajs dartea elm react-hooks svelte solid}"
if [ ! -d "$work" ]; then
  git clone --depth 1 https://github.com/krausest/js-framework-benchmark.git "$work"
  (cd "$work" && npm ci && npm run install-local)
fi
rm -rf "$work/frameworks/keyed/dartea"
mkdir -p "$work/frameworks/keyed/dartea/dist"
cp -R "$here/package.json" "$here/package-lock.json" "$here/index.html" "$here/src" "$work/frameworks/keyed/dartea/"
"$here/../_build/default/bin/main.exe" make "$work/frameworks/keyed/dartea/src/Main.elm" --output "$work/frameworks/keyed/dartea/dist/main.js"
for f in $frameworks; do
  [ "$f" = dartea ] && continue
  [ "$f" = vanillajs ] && continue
  (cd "$work/frameworks/keyed/$f" && npm ci && npm run build-prod)
done
(cd "$work/server" && npm start > /dev/null 2>&1 &)
sleep 3
(cd "$work/webdriver-ts" && npm run bench -- --headless --framework $(for f in $frameworks; do printf "keyed/%s " "$f"; done) && npm run results)
echo "table: $work/webdriver-ts-results/dist/index.html"
