type token = { t : Token.t; line : int }
type state

(** [default s] returns a default [state] from a string. *)
val default : string -> state

val advance : state -> state

(** [is_at_end s] is true when [current] has reached past [String.length
    source]. *)
val is_at_end : state -> bool

val get_current_char : state -> char
val add_token : state -> Token.t -> state
val add_two_char_token : state -> Token.t -> Token.t -> state
val advance_past : state -> (char -> bool) -> state
val substring : state -> string option
val float_substring : state -> state
val scan_token : state -> string Error.result * state
val scan_tokens : string -> (token list, string list) result
