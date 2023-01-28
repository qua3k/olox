type t =
  (* Single-character tokens. *)
  | LEFT_PAREN
  | RIGHT_PAREN
  | LEFT_BRACE
  | RIGHT_BRACE
  | COMMA
  | DOT
  | MINUS
  | PLUS
  | SEMICOLON
  | SLASH
  | STAR
  (* One or two character tokens. *)
  | BANG
  | BANG_EQUAL
  | EQUAL
  | EQUAL_EQUAL
  | GREATER
  | GREATER_EQUAL
  | LESS
  | LESS_EQUAL
  (* Literals. *)
  | IDENTIFIER
  | STRING of string
  | NUMBER of float
  (* Keywords. *)
  | AND
  | CLASS
  | ELSE
  | FALSE
  | FUN
  | FOR
  | IF
  | NIL
  | OR
  | PRINT
  | RETURN
  | SUPER
  | THIS
  | TRUE
  | VAR
  | WHILE
  (* EOF *)
  | EOF

let to_string = function
  (* Single-character tokens. *)
  | LEFT_PAREN -> "("
  | RIGHT_PAREN -> ")"
  | LEFT_BRACE -> "{"
  | RIGHT_BRACE -> "}"
  | COMMA -> ","
  | DOT -> "."
  | MINUS -> "-"
  | PLUS -> "+"
  | SEMICOLON -> ";"
  | SLASH -> "/"
  | STAR -> "*"
  (* One or two character tokens. *)
  | BANG -> "!"
  | BANG_EQUAL -> "!="
  | EQUAL -> "="
  | EQUAL_EQUAL -> "=="
  | GREATER -> ">"
  | GREATER_EQUAL -> ">="
  | LESS -> "<"
  | LESS_EQUAL -> "<="
  (* Literals. *)
  | IDENTIFIER -> "identifier"
  | STRING s -> "string " ^ s
  | NUMBER f -> "number " ^ Float.to_string f
  (* Keywords. *)
  | AND -> "and"
  | CLASS -> "class"
  | ELSE -> "else"
  | FALSE -> "false"
  | FUN -> "fun"
  | FOR -> "for"
  | IF -> "if"
  | NIL -> "nil"
  | OR -> "or"
  | PRINT -> "print"
  | RETURN -> "return"
  | SUPER -> "super"
  | THIS -> "this"
  | TRUE -> "true"
  | VAR -> "var"
  | WHILE -> "while"
  | EOF -> "eof"
