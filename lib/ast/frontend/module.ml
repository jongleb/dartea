type top_level =
  | Type_alias of Typealias.t
  | Type_dec of Typedecl.t
  | Top_declaration of Declaration.t  (** fixme: rename it *)
[@@deriving show]

type t = {
  imports : Import_thing.t list;
  top_levels : top_level list;
  exports : Exposing.t;
  name : string Data.Located.t;
}
[@@deriving show]

let of_impl =
  List.fold_left
    (fun acc next ->
      match next with
      | Impl.Import thing ->
          { acc with imports = List.concat [ acc.imports; [ thing ] ] }
      | Impl.Type_alias ta ->
          {
            acc with
            top_levels = List.concat [ acc.top_levels; [ Type_alias ta ] ];
          }
      | Impl.Type_dec td ->
          {
            acc with
            top_levels = List.concat [ acc.top_levels; [ Type_dec td ] ];
          }
      | Impl.Top_declaration td ->
          {
            acc with
            top_levels = List.concat [ acc.top_levels; [ Top_declaration td ] ];
          }
      | Impl.Export e -> { acc with exports = e }
      | Impl.ModuleName name -> { acc with name })
    {
      imports = [];
      top_levels = [];
      exports = Exposing.Open;
      name = Data.Located.(~?"");
    }
