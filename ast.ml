type literal = NIL | BOOL of bool | NUMBER of float | STRING of string

type expression =
  | ASSIGNMENT of { ident : literal; equal : Token.t; expr : expression }
  | BINARY of { left : expression; operator : Token.t; right : expression }
  | GROUPING of { left : Token.t; expr : expression; right : Token.t }
  | LITERAL of literal
  | UNARY of { operator : Token.t; expr : expression }
