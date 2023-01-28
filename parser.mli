val not_at_end : Lexer.token -> bool
val consume : Token.t -> Lexer.token list -> Lexer.token list option
val synchronize : Lexer.token list -> Lexer.token list
val parse : Lexer.token list -> Ast.expression
module Eval :
  sig
    val is_true : Ast.literal -> bool
    val is_equal : Ast.literal -> Ast.literal -> bool
    val unary : Token.t -> Ast.expression -> Ast.literal
    val binary : Ast.expression -> Token.t -> Ast.expression -> Ast.literal
    val interpret : Ast.expression -> (Ast.literal, exn) result
  end
