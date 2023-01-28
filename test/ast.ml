(** Test proper AST generation. *)

open Olox.Ast
open Olox.Print.Ast
open Olox.Token

let example =
  BINARY
    {
      left = UNARY { operator = MINUS; expr = LITERAL (NUMBER 123.) };
      operator = STAR;
      right =
        GROUPING
          {
            left = LEFT_PAREN;
            expr = LITERAL (NUMBER 45.67);
            right = RIGHT_PAREN;
          };
    }

let () =
  match print example with
  | "(* (- 123.) (group 45.67))" -> exit 0
  | _ -> exit ~-1
