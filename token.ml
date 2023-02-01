type t =
  (* Single-character tokens. *)
  | Left_paren
  | Right_paren
  | Left_brace
  | Right_brace
  | Comma
  | Dot
  | Minus
  | Plus
  | Semicolon
  | Slash
  | Star
  (* One or two character tokens. *)
  | Bang
  | Bang_equal
  | Equal
  | Equal_equal
  | Greater
  | Greater_equal
  | Less
  | Less_equal
  (* Literals. *)
  | Identifier
  | String of string
  | Number of float
  (* Keywords. *)
  | And
  | Class
  | Else
  | False
  | Fun
  | For
  | If
  | Nil
  | Or
  | Print
  | Return
  | Super
  | This
  | True
  | Var
  | While
  (* EOF *)
  | Eof

let to_string = function
  (* Single-character tokens. *)
  | Left_paren -> "("
  | Right_paren -> ")"
  | Left_brace -> "{"
  | Right_brace -> "}"
  | Comma -> ","
  | Dot -> "."
  | Minus -> "-"
  | Plus -> "+"
  | Semicolon -> ";"
  | Slash -> "/"
  | Star -> "*"
  (* One or two character tokens. *)
  | Bang -> "!"
  | Bang_equal -> "!="
  | Equal -> "="
  | Equal_equal -> "=="
  | Greater -> ">"
  | Greater_equal -> ">="
  | Less -> "<"
  | Less_equal -> "<="
  (* Literals. *)
  | Identifier -> "identifier"
  | String s -> "string " ^ s
  | Number f -> "number " ^ Float.to_string f
  (* Keywords. *)
  | And -> "and"
  | Class -> "class"
  | Else -> "else"
  | False -> "false"
  | Fun -> "fun"
  | For -> "for"
  | If -> "if"
  | Nil -> "nil"
  | Or -> "or"
  | Print -> "print"
  | Return -> "return"
  | Super -> "super"
  | This -> "this"
  | True -> "true"
  | Var -> "var"
  | While -> "while"
  | Eof -> "eof"
