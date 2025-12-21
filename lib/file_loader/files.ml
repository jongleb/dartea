open Eio.Path
open Eio.Buf_read.Syntax

module Elm_file = struct
  type t = { path : string; content : string }
end

let current_folder (root_dir : _ Eio.Path.t) =
  root_dir |> Eio.Path.read_dir
  |> List.filter_map (fun file_name ->
         if String.ends_with file_name ~suffix:".elm" then
           Some
             Elm_file.
               {
                 path = file_name;
                 content = Eio.Path.load (root_dir / file_name);
               }
         else None)
