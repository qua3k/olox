val not_at_end : Lexer.token -> bool
val consume : Token.t -> Lexer.token list -> Lexer.token list option
val synchronize : Lexer.token list -> Lexer.token list

module Expression : sig
  val expression : Lexer.token list -> Ast.expression * Lexer.token list
end

module Statement : sig
  val parse :
    Lexer.token list ->
    Ast.statement list ->
    (Ast.statement list, string) result
end

module Eval : sig
  val is_true : Ast.literal -> bool
  val is_equal : Ast.literal -> Ast.literal -> bool
  val unary : Token.t -> Ast.expression -> Ast.literal
  val binary : Ast.expression -> Token.t -> Ast.expression -> Ast.literal
  val print : Ast.expression -> unit
  val statement : Ast.statement -> unit
end

val interpret : Lexer.token list -> (unit, string) result
