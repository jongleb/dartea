let folder () = Filename.temp_dir "dartea" ""

let rec ensured directory =
  if not (Sys.file_exists directory) then begin
    ensured (Filename.dirname directory);
    Sys.mkdir directory 0o755
  end

let written ~folder ~path content =
  let file = Filename.concat folder path in
  ensured (Filename.dirname file);
  Out_channel.with_open_bin file (fun out ->
      Out_channel.output_string out content)

let names spelled = String.concat ", " spelled
let sorted spelled = List.sort String.compare spelled

let rendered seen (error : Reporting.Error.t) =
  Reporting.Report.to_string ~colours:false
    (Reporting.Sources.report seen error)

let starter = {|module Main exposing (main)


main : String
main =
    String.fromInt (2 + 3) ++ " from dartea"
|}
