(** Test proper AST generation. *)

open Olox.Ast
open Olox.Print.Ast
open Olox.Token

let example =
  Binary
    {
      left = Unary { operator = Minus; expr = Literal (Number 123.) };
      operator = Star;
      right = Grouping (Literal (Number 45.67));
    }

let () = assert (expression example = "(* (- 123.) (group 45.67))")
