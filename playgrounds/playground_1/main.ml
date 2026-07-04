let () =
  Eio_main.run @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in
  let path = Eio.Path.(cwd / "playgrounds" / "elm_code") in
  Dartea.Compiler.compile path
