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

let started = "Dartea"

let of_modules ~entry_module ~declaration modules =
  Printf.sprintf
    "%s\n\n%s\nglobalThis.%s = {\n  %s: {\n    init: (config) =>\n      %s(%s.%s, config.node, config.flags),\n  },\n};\n"
    (String.concat "\n\n" (List.map wrapped modules))
    Runtime.engine_source started
    (Of_optimized.module_ident entry_module)
    Runtime.mount
    (Of_optimized.module_ident entry_module)
    declaration

let sandwich ~title ~entry_module script =
  Printf.sprintf
    {|<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8" />
    <title>%s</title>
    <style>body { padding: 0; margin: 0; }</style>
  </head>
  <body>
    <div id="main"></div>
    <script>
try {
%s
  %s.%s.init({ node: document.getElementById("main") });
} catch (thrown) {
  const header = document.createElement("h1");
  header.style.fontFamily = "monospace";
  header.innerText = "Initialization Error";
  const holder = document.getElementById("main");
  document.body.insertBefore(header, holder);
  holder.innerText = thrown;
  throw thrown;
}
    </script>
  </body>
</html>
|}
    title script started (Of_optimized.module_ident entry_module)
