type module_ = { name : string; source : string }

let entry_file = "main.js"
let page_file = "index.html"

let without_prefix ~prefix line =
  String.sub line (String.length prefix)
    (String.length line - String.length prefix)

let body source =
  let exported = "export" in
  String.split_on_char '\n' source
  |> List.filter_map (fun line ->
         if String.starts_with ~prefix:"import " line then None
         else if String.starts_with ~prefix:exported line then
           Some ("return" ^ without_prefix ~prefix:exported line)
         else Some line)
  |> String.concat "\n" |> String.trim

let wrapped { name; source } =
  Printf.sprintf "const %s = (() => {\n%s\n})();"
    (Of_optimized.module_ident name)
    (body source)

let of_modules ~entry_module ~declaration modules =
  Printf.sprintf "%s\nexport const %s = %s.%s;\n"
    (String.concat "\n\n" (List.map wrapped modules))
    declaration
    (Of_optimized.module_ident entry_module)
    declaration

let page ~title ~declaration =
  Printf.sprintf
    {|<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8" />
    <title>%s</title>
  </head>
  <body>
    <div id="main"></div>
    <script type="module">
      import { %s } from "./%s";
      document.getElementById("main").textContent = %s;
    </script>
  </body>
</html>
|}
    title declaration entry_file declaration
