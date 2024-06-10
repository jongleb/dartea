open Eio.Path
open Eio.Buf_read.Syntax

module Elm_file = struct
  type t = { path : string; content : string }
end

let current_folder root_path =
  Eio_main.run (fun env ->
      let root_dir = Eio.Stdenv.cwd env / root_path in
      let files =
        root_dir |> Eio.Path.read_dir
        (*
            do it recrusively
            https://ocaml-multicore.github.io/eio/eio/Eio/Path/index.html#metadata
         *)
        |> List.filter_map (fun file_name ->
               if String.ends_with file_name ~suffix:".elm" then
                 Some
                   Elm_file.
                     {
                       path = root_path ^ "/" ^ file_name;
                       content = Eio.Path.load @@ (root_dir / file_name);
                     }
               else None)
      in
      files)
