module Occurrence = After_typed.Exhaustive.Occurrence
module Decision_tree = After_typed.Exhaustive.Decision_tree

type atom =
  | A_var of string
  | A_int of int
  | A_float of float
  | A_string of string
  | A_char of string
  | A_unit
  | A_nil
  | A_constant of Data.Name.t
  | A_global of Data.Name.t

type bind =
  | B_atom of atom
  | B_construct of { name : Data.Name.t; arguments : atom list }
  | B_cons of { head : atom; tail : atom }
  | B_tuple of { items : atom list }
  | B_record of { fields : (string * atom) list }
  | B_record_update of { base : atom; fields : (string * atom) list }
  | B_access of { subject : atom; step : Occurrence.step }
  | B_call of { callee : Data.Name.t; arguments : atom list }
  | B_call_closure of { callee : atom; arguments : atom list }
  | B_partial of { callee : Data.Name.t; arguments : atom list; missing : int }
  | B_primitive of { operator : Data.Operator.t; arguments : atom list }
  | B_kernel of { kernel : Data.Kernel.t; arguments : atom list }
  | B_closure of {
      parameters : string list;
      captures : string list;
      body : term;
    }

and term =
  | T_return of atom
  | T_let of { name : string; bind : bind; body : term }
  | T_if of { condition : atom; consequent : term; alternative : term }
  | T_switch of {
      subject : atom;
      branches : (Decision_tree.test * term) list;
      default : term option;
    }
  | T_join of {
      label : string;
      parameters : string list;
      definition : term;
      body : term;
    }
  | T_jump of { label : string; arguments : atom list }
  | T_fail of { message : string }

type declaration = { name : string; parameters : string list; body : term }
