open Packages

type endpoint = { url : string; hash : string }
[@@deriving of_yojson { strict = false }]

let endpoint client (pick : Pick.t) =
  let url =
    String.concat "/"
      [
        "https://package.elm-lang.org/packages";
        pick.package;
        Version.show pick.version;
        "endpoint.json";
      ]
  in
  let unreadable _ =
    Store.refuse pick ("I cannot read what " ^ url ^ " answered")
  in
  let read content =
    match Yojson.Safe.from_string content with
    | json -> Result.fold ~ok:Fun.id ~error:unreadable (endpoint_of_yojson json)
    | exception Yojson.Json_error problem -> unreadable problem
  in
  Option.map read (Fetch.body client ~accept:"application/json" url)
