module Import : sig
  (** Declarations of an imported module, with the names it exports. *)
  type t = {
    module_name : string;
    exports : Data.Name.t list;
    declarations : Optimized.Declaration.t list;
  }
end

val optimize :
  imports:Import.t list ->
  Typed.Declaration.t list ->
  Optimized.Declaration.t list
