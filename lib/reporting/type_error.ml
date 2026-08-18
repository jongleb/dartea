type thing = A_type | A_variant [@@deriving show]

type t =
  | Bad_expression of {
      category : Category.t;
      found : Typed.Type.t;
      expected : Expectation.t;
    }
  | Bad_pattern of {
      category : Category.pattern;
      found : Typed.Type.t;
      expected : Expectation.pattern;
    }
  | Infinite_type of { category : Category.t; found : Typed.Type.t }
  | Bad_arity of {
      thing : thing;
      name : Data.Name.t;
      expects : int;
      given : int;
    }
  | Case_without_branches
[@@deriving show]
