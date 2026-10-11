(lang dune 3.10)

(using menhir 2.0)

(name dartea)

(generate_opam_files true)

(source
 (github jongleb/dartea))

(authors "Gleb <10594395+jongleb@users.noreply.github.com>")

(maintainers "Gleb <10594395+jongleb@users.noreply.github.com>")

(license BSD-3-Clause)

(package
 (name dartea)
 (synopsis "Elm compiler. Modern JavaScript output. No virtual DOM.")
 (description
  "dartea compiles Elm to modern JavaScript. ES modules, async and await, fetch, AbortSignal. Dead code is removed across modules. The compiler owns the Html library and the runtime. It turns the static part of a view into a DOM template and updates only the values that changed. There is no virtual DOM diff. On js-framework-benchmark the output is close to Solid and Svelte and ahead of React. dartea is an independent project and is not endorsed by Elm. See LICENSE for the files that come from elm/core, elm/html, elm/browser, elm/url and elm/http.")
 (depends
  (ocaml
   (>= 5.4.0))
  dune
  (menhir
   (>= 20240715))
  (cmdliner
   (>= 2.0.0))
  fpath
  bos
  rresult
  (yojson
   (>= 3.0.0))
  conf-libcurl
  ezcurl
  digestif
  base64
  (tar
   (>= 3.0.0))
  (tar-unix
   (>= 3.0.0))
  decompress
  ppx_deriving
  ppx_jane
  non_empty_list
  base
  (ounit2 :with-test)
  (qcheck-core :with-test)
  (qcheck-ounit :with-test))
 (tags
  (compiler elm javascript)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
