val not_at_end : Lexer.token -> bool
val consume : Token.t -> Lexer.token list -> Lexer.token list option

val match_token :
  Lexer.token list -> Token.t -> (Token.t * Lexer.token list) option

val match_tokens :
  Lexer.token list -> Token.t list -> (Token.t * Lexer.token list) option

val match_right :
  Token.t * Lexer.token list ->
  Token.t list ->
  (Lexer.token list -> 'a * 'b) ->
  Token.t * 'a * 'b

val match_tree :
  Lexer.token list ->
  Token.t list ->
  (Lexer.token list -> Ast.expression * Lexer.token list) ->
  Ast.expression * Lexer.token list

val equality : Lexer.token list -> Ast.expression * Lexer.token list
val expression : Lexer.token list -> Ast.expression * Lexer.token list
val comparison : Lexer.token list -> Ast.expression * Lexer.token list
val term : Lexer.token list -> Ast.expression * Lexer.token list
val factor : Lexer.token list -> Ast.expression * Lexer.token list
val unary : Lexer.token list -> Ast.expression * Lexer.token list
val primary : Lexer.token list -> Ast.expression * Lexer.token list
val synchronize : Lexer.token list -> Lexer.token list
val parse : Lexer.token list -> Ast.expression * Lexer.token list
