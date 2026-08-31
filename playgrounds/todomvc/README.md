# TodoMVC on dartea

The upstream elm-todomvc program, unchanged, compiled by dartea.

```
eval $(opam env --switch=test54 --set-switch)
dune build
cd playgrounds/todomvc
../../_build/default/bin/main.exe make Main.elm --output build/app.js
open index.html
```

`index.html` loads `style.css` and `build/app.js`, restores the todos from `localStorage` through the `setStorage` port and starts the app. Use any local web server instead of `open` if the browser complains about `file:` URLs.
