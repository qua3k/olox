type token = { t : Token.t; line : int }
type state

val default_state : string -> state
val advance : state -> state
val is_at_end : state -> bool
val get_current_char : state -> char
val add_token : state -> Token.t -> state
val add_two_char_token : state -> Token.t -> Token.t -> state
val advance_past : state -> (char -> bool) -> state
val state_substring : state -> string option
val state_substring_float : state -> state
val scan_token : state -> state
val scan_tokens : string -> token list
