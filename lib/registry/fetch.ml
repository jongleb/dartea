type answer = Found of string | Absent | Broken of string

let agent = "dartea"
let hops = 5

let settings =
  Ezcurl.Config.(max_redirects hops (follow_location true default))

let judge url (response : string Ezcurl.response) =
  match response.code with
  | 200 -> Found response.body
  | 404 -> Absent
  | code -> Broken (Printf.sprintf "%s answered %d" url code)

let get client ~accept url =
  let headers = [ ("accept", accept); ("user-agent", agent) ] in
  match Ezcurl.get ~client ~config:settings ~headers ~url () with
  | Ok response -> judge url response
  | Error (_, problem) -> Broken problem
