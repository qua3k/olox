type literal = Nil | Bool of bool | Number of float | String of string

type expression =
  | Assign of { name : string; value : expression }
  | Binary of { left : expression; operator : Token.t; right : expression }
  | Grouping of expression
  | Literal of literal
  | Unary of { operator : Token.t; expr : expression }
  | Variable of string

type statement =
  | Block of statement list
  | Expression of expression
  | Print of expression
  | Var of { name : string; init : expression option }
