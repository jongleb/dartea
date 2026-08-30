type module_ = { name : string; source : string }

let entry_file = "main.js"
let page_file = "index.html"

let body source =
  String.split_on_char '\n' source
  |> List.filter_map (fun line ->
         if String.starts_with ~prefix:"import " line then None
         else
           let keyword = "export" in
           if String.starts_with ~prefix:keyword line then
             Some
               ("return"
               ^ String.sub line (String.length keyword)
                   (String.length line - String.length keyword))
           else Some line)
  |> String.concat "\n" |> String.trim

let wrap { name; source } =
  Printf.sprintf "const %s = (() => {\n%s\n})();"
    (Of_optimized.module_ident name)
    (body source)

let global_name = "Dartea"

let of_modules ~entry_module ~declaration ~flags modules =
  Printf.sprintf
    "%s\n\n%s\nglobalThis.%s = {\n  %s: {\n    init: (config) =>\n      %s(%s.%s, config.node, (%s)(config.flags, \"flags\")),\n  },\n};\n"
    (String.concat "\n\n" (List.map wrap modules))
    Runtime.engine_source global_name
    (Of_optimized.module_ident entry_module)
    Runtime.mount
    (Of_optimized.module_ident entry_module)
    declaration (To_string.expr_to_string flags)

let sandwich ~title ~entry_module ~notice script =
  let heading = Option.fold ~none:"" ~some:(fun text -> "<!--\n" ^ text ^ "-->\n") notice in
  Printf.sprintf
    {|<!DOCTYPE html>
%s<html lang="en">
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
    heading title script global_name (Of_optimized.module_ident entry_module)
