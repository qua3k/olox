type token = { t : Token.t; line : int }

(** The record for the lexer state. *)
type state

(** [default s] returns a default [state] from a string. *)
val default : string -> state

val advance : state -> state

(** [is_at_end s] is true when [current] has reached past [String.length
    source]. *)
val is_at_end : state -> bool

val get_current_char : state -> char
val emit_two_char_token : state -> 'a -> 'a -> 'a
val advance_past : (char -> bool) -> state -> state
val substring : state -> string option

(** Get a token or an error from one state. *)
val emit_token : state -> (Token.t, string) result * state

(** Lex a Lox program into tokens and errors. *)
val scan_tokens : string -> (token list, string list) result
