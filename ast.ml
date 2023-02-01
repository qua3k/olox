type literal = Nil | Bool of bool | Number of float | String of string

type expression =
  | Assignment of { ident : literal; equal : Token.t; expr : expression }
  | Binary of { left : expression; operator : Token.t; right : expression }
  | Grouping of expression
  | Literal of literal
  | Unary of { operator : Token.t; expr : expression }

type statement = Expression of expression | Print of expression
