val token_error : Lexer.token -> string -> unit
val consume : Token.t -> Lexer.token list -> Lexer.token list option

val match_tokens :
  Lexer.token list -> Token.t list -> (Token.t * Lexer.token list) option

val match_right :
  Token.t * Lexer.token list ->
  Token.t list ->
  (Lexer.token list -> 'a * 'b) ->
  Token.t * 'a * 'b

val synchronize : Lexer.token list -> Lexer.token list

module Expression : sig
  val match_tree :
    Lexer.token list ->
    Token.t list ->
    (Lexer.token list -> Ast.expression * Lexer.token list) ->
    Ast.expression * Lexer.token list

  val primary : Lexer.token list -> Ast.expression * Lexer.token list
  val unary : Lexer.token list -> Ast.expression * Lexer.token list
  val factor : Lexer.token list -> Ast.expression * Lexer.token list
  val term : Lexer.token list -> Ast.expression * Lexer.token list
  val comparison : Lexer.token list -> Ast.expression * Lexer.token list
  val equality : Lexer.token list -> Ast.expression * Lexer.token list
  val assignment : Lexer.token list -> Ast.expression * Lexer.token list
  val expression : Lexer.token list -> Ast.expression * Lexer.token list
end

module Eval : sig
  val is_true : Ast.literal -> bool
  val is_equal : Ast.literal -> Ast.literal -> bool

  val evaluate :
    (string, Ast.literal option) Hashtbl.t list ->
    Ast.expression ->
    Ast.literal

  val unary :
    (string, Ast.literal option) Hashtbl.t list ->
    Token.t ->
    Ast.expression ->
    Ast.literal

  val binary :
    (string, Ast.literal option) Hashtbl.t list ->
    Ast.expression ->
    Token.t ->
    Ast.expression ->
    Ast.literal

  val variable :
    (string, Ast.literal option) Hashtbl.t list -> string -> Ast.literal

  val assign :
    (string, Ast.literal option) Hashtbl.t list ->
    string ->
    Ast.expression ->
    Ast.literal

  val print :
    (string, Ast.literal option) Hashtbl.t list -> Ast.expression -> unit

  val statement :
    (string, Ast.literal option) Hashtbl.t list -> Ast.statement -> unit

  val execute_block :
    (string, Ast.literal option) Hashtbl.t list ->
    Ast.statement list ->
    unit
end

module Statement : sig
  val statement :
    Lexer.token list -> (Lexer.token list * Ast.statement, string) result

  val print :
    Lexer.token list -> (Lexer.token list * Ast.statement, string) result

  val block :
    Lexer.token list ->
    Ast.statement list ->
    (Lexer.token list * Ast.statement, string) result

  val expression :
    Lexer.token list -> (Lexer.token list * Ast.statement, string) result

  val var :
    Lexer.token list -> (Lexer.token list * Ast.statement, string) result

  val var_statement :
    Lexer.token list -> (Lexer.token list * Ast.statement, string) result

  val declaration :
    Lexer.token list -> (Ast.statement, string) result * Lexer.token list

  val parse_tokens :
    Lexer.token list -> (Ast.statement list, string list) result
end

val interpret : Lexer.token list -> (unit, string list) result
