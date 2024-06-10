type t =
  | Expr_char of string (* elm type: Chr ES.String, NEED IMPLEMENT *)
  | Expr_string of string
  (* Chr ES.String *)
  | Expr_int of int
  | Expr_float of float (* EF.Float *)
  | Expr_list of t list
  | Expr_constr of expr_constr
  | Expr_binop of expr_binop
  (* | Expr_let of expr_let
     | Expr_if_then_else of expr_if_then_else *)
  | Expr_record of expr_record_row list
  | Expr_apply of expr_apply
  | Expr_ident of expr_ident
  (* | Expr_pattern of expr_pattern
     | Expr_accessor of string Data.Located.t*)
  | Expr_access of expr_access
[@@deriving show]

and expr_constr = { name : string; arguments : t list; module_name : string }
[@@deriving show]
(*ConstructorValue { qualifiedness : PossiblyQualified, name : VarName }*)

and expr_binop = { name : string; operands : t * t } [@@deriving show]
(*  Binops [(Expr, A.Located Name)] Expr *)

and expr_record_row = { name : string; value : t; module_name : string }
[@@deriving show]

and expr_apply = { ident : t; args : t list } [@@deriving show]
and expr_ident = { name : string; module_name : string } [@@deriving show]
(* and expr_pattern_case = { pattern : Pattern.t; expr : t } [@@deriving show]

   and expr_pattern = { expr : t; pattern_data_items : expr_pattern_case list }
   [@@deriving show] *)

and expr_access = { expr : t; field : string Data.Located.t } [@@deriving show]

module Str_set = Set.Make (String)

type canonicalize_env = { constrs : Str_set.t }

let rec of_frontend exports env expr =
  let rec go env = function
    | Frontend.Expr.Expr_float i -> Expr_float i
    | Expr_string s -> Expr_string s
    | Expr_constr c -> (
        let find_by_constr_name = function
          | Frontend.Exposing.Upper upper
            when Data.Located.unwrap upper.name = c.name ->
              Some c.name
          | _ -> None
        in

        let result =
          match exports with
          | Frontend.Exposing.Open -> None
          | Explicit lst -> List.find_map find_by_constr_name lst
        in

        match result with
        | Some r ->
            Expr_constr
              {
                name = c.name;
                arguments = List.map (go env) c.Frontend.Expr.arguments;
                module_name = "";
              }
        | None -> failwith "..."
        (* env.constrs |> Str_set.find_opt c.name
           |> Option.value
                ~default:
                  ((* let result = Str_set.se *)
                     Expr_constr
                     {
                       name = c.name;
                       arguments = List.map (go env) c.Frontend.Expr.arguments;
                       module_name = "";
                     }) *))
    (* | Expr_apply { ident; args } -> (
        let find exposing =
          match (exposing, ident) with
          | Frontend.Exposing.Lower lower, Expr_ident ident
            when Data.Located.unwrap lower = ident ->
              Some ident
          | _ -> None
        in
        let result =
          match exports with
          | Frontend.Exposing.Open -> None
          | Explicit lst -> List.find_map find lst
        in
        match (result, ident) with
        | Some r, Expr_ident ident ->
            Expr_apply
              {
                ident = Expr_ident { name = ident; module_name = "" };
                args = List.map (go env) args;
              }
        | _ -> failwith "...") *)
    | Expr_access { expr; field } -> Expr_access { expr = go env expr; field }
    | Expr_list list -> Expr_list (List.map (go env) list)
    | Expr_binop { name; operands = a, b } ->
        Expr_binop { name; operands = (go env a, go env b) }
    (* | Expr_let exp -> Expr_let exp *)
    | _ -> failwith @@ "..."
  in
  go env expr
