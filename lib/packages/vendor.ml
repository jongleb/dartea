let folder = Files.Relative.of_string ".dartea/packages"
let cache = Files.Relative.of_string ".dartea/cache"
let sources = "src"
let archive = ".tgz"

let at pick =
  Files.Relative.inside folder (Files.Relative.of_string (Pick.path pick))

let inside pick = Files.Relative.extended (at pick) sources

let tarball pick =
  Files.Relative.inside cache
    (Files.Relative.of_string (Pick.path pick ^ archive))
