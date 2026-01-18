let () =
  Eio_main.run @@ fun env ->
  let path =
    if Array.length Sys.argv > 1
    then Eio.Path.(Eio.Stdenv.fs env / Sys.argv.(1))
    else Eio.Path.(Eio.Stdenv.fs env / ".")
  in
  Dartea.Compiler.compile path
