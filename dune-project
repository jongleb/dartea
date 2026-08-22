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
 (synopsis "A compiler for the Elm language, emitting modern JavaScript")
 (description
  "An independent implementation of the Elm language in OCaml, with a target-neutral core and a JavaScript backend. Not affiliated with or endorsed by the Elm project. Contains material derived from elm/core and elm/compiler; see the LICENSE file.")
 (depends ocaml dune)
 (tags
  (compiler elm javascript)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
