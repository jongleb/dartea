exception Offline of { url : string; problem : string }

let agent = "dartea"
let hops = 5
let anything = "*/*"

let settings =
  Ezcurl.Config.(max_redirects hops (follow_location true default))

let body client ~accept url =
  let headers = [ ("accept", accept); ("user-agent", agent) ] in
  match Ezcurl.get ~client ~config:settings ~headers ~url () with
  | Ok { code = 200; body; _ } -> Some body
  | Ok { code = 404; _ } -> None
  | Ok { code; _ } ->
      raise
        (Offline { url; problem = Printf.sprintf "%s answered %d" url code })
  | Error (_, problem) -> raise (Offline { url; problem })
