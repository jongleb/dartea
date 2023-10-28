open Sexplib.Std

type record_data = {
    key: string;
    value: type_alias_data;
} [@@deriving show]

and record_typ = {
    values: record_data list;
    row_type: string option;
} [@@deriving show]

and function_typ = {
    arguments: type_alias_data list;
} [@@deriving show]

and type_alias_kind = 
    | Concrete of string
    | Type_var of string
    | Record of record_typ
    | Tuples of type_alias_data list
    | Function of function_typ
    [@@deriving show]

and type_alias_data = {
    params: type_alias_data list;
    content: type_alias_kind;
}[@@deriving show]

type type_alias_desc = {
    data: type_alias_data;
    params: string list;
    id: string;
} [@@deriving show]

type elm_ast =
    | Type_alias of type_alias_desc
    [@@deriving show]
