(*
   This is the file containing all the types, the tokens and the AST of a jack class
*)
module Token = struct

  type t = Keyword of string
         | Symbol of char
         | IntConst of int
         | StringConst of string
         | Identifier of string

  (*Helper function to show a token in the error messages*)
  let to_string = function
    | Keyword k -> k
    | Symbol c -> String.make 1 c
    | IntConst n -> string_of_int n
    | StringConst s -> "\"" ^ s ^ "\""
    | Identifier i -> i

end

module Expr = struct

  (*An expression is a term followed by a list of (op, term), ex=> x + 1 -> (Var "x", [('+', IntConst 1)])*)
  type t = term * (char * term) list
  and term = IntConst of int
           | StringConst of string
           | KeywordConst of string
           | Var of string
           | Index of string * t
           | Call of call
           | Paren of t
           | Unary of char * term
  (*A subroutine call, target is the class or the variable before the '.', ex=> Output.printInt(x)*)
  and call = { target : string option;
               name   : string;
               args   : t list;
             }

end

module Statement = struct

  type t = Let of string * Expr.t option * Expr.t
         | If of Expr.t * t list * t list option
         | While of Expr.t * t list
         | Do of Expr.call
         | Return of Expr.t option

end

module Class = struct

  type var_kind = Static | Field
  type sub_kind = Constructor | Function | Method

  (*A class variable declaration, ex.=>  field int x, y; -> (Field, "int", ["x"; "y"])*)
  type var_dec = var_kind * string * string list

  type subroutine = { sub_kind    : sub_kind;
                      return_type : string;
                      sub_name    : string;
                      params      : (string * string) list;       (*(type, name)*)
                      locals      : (string * string list) list;  (*(type, names)*)
                      body        : Statement.t list;
                    }

  type t = { class_name  : string;
             class_vars  : var_dec list;
             subroutines : subroutine list;
           }

end

