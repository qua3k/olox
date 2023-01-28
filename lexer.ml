let report line where message =
  prerr_endline
    (Printf.sprintf "[line %s] Error %s : %s" line where message)

let error line message = report (string_of_int line) "" message

type token = { t : Token.t; line : int }

type state = {
  source : string;
  tokens : token list;
  start : int;
  current : int;
  line : int;
}

let default_state source =
  { source; tokens = []; start = 0; current = 0; line = 1 }

let advance state = { state with current = state.current + 1 }
let is_at_end state = state.current >= String.length state.source

let get_current_char state =
  try state.source.[state.current] with _ -> '\x00'

let add_token state t =
  { state with tokens = { t; line = state.line } :: state.tokens }

let add_two_char_token next a b =
  let token =
    match get_current_char next = '=' with true -> a | false -> b
  in
  add_token (advance next) token

(** Advance state to end as long as the predicate is true, setting current
    to the first char that doesn't meet the predicate. *)
let rec advance_past state (fn : char -> bool) =
  match is_at_end state with
  | true -> state
  | false -> (
      let ch = get_current_char state in
      match fn ch with
      | true -> advance_past (advance state) fn
      | false -> state)

let add_slash next =
  let next_ch = get_current_char next in
  match next_ch with
  | '/' -> advance_past next (fun c -> c != '\n')
  (* | '*' -> If we allow nested comments we need to keep count of how deep
     we're nesting. *)
  | _ -> add_token next SLASH

let is_digit = function '0' .. '9' -> true | _ -> false

let is_alpha = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false

let is_alpha_numeric ch = is_alpha ch || is_digit ch

let state_substring state =
  try
    let start = state.start in
    Some (String.sub state.source start (state.current - start))
  with _ -> None

let state_substring_float state =
  match Option.bind (state_substring state) Float.of_string_opt with
  | Some f -> add_token state (NUMBER f)
  | None -> state

let handle_string next =
  let state = advance_past next (fun c -> c != '"') in
  match is_at_end state with
  | true ->
      error next.line "Unterminated string.";
      next
  | false -> (
      let after = advance state in
      match state_substring after with
      | Some s -> add_token after (STRING s) (* advances past last quote *)
      | None -> next)

let handle_digit state =
  let next = advance_past state is_digit in
  match get_current_char next with
  | '.' -> (
      let after = advance next in
      match after |> get_current_char |> is_digit with
      | true -> advance_past after is_digit |> state_substring_float
      | false -> state_substring_float next)
  | _ -> state_substring_float next

let handle_identifier state =
  let next = advance_past state is_alpha_numeric in
  match state_substring next with
  | Some s ->
      let open Token in
      let token_type =
        match s with
        | "and" -> AND
        | "class" -> CLASS
        | "else" -> ELSE
        | "false" -> FALSE
        | "fun" -> FUN
        | "for" -> FOR
        | "if" -> IF
        | "nil" -> NIL
        | "or" -> OR
        | "print" -> PRINT
        | "return" -> RETURN
        | "super" -> SUPER
        | "this" -> THIS
        | "true" -> TRUE
        | "var" -> VAR
        | "while" -> WHILE
        | _ -> IDENTIFIER
      in
      add_token next token_type
  | None -> next

let scan_token state =
  let next = advance state in
  match get_current_char state with
  | '(' -> add_token next LEFT_PAREN
  | ')' -> add_token next RIGHT_PAREN
  | '{' -> add_token next LEFT_BRACE
  | '}' -> add_token next RIGHT_BRACE
  | ',' -> add_token next COMMA
  | '.' -> add_token next DOT
  | '-' -> add_token next MINUS
  | '+' -> add_token next PLUS
  | ';' -> add_token next SEMICOLON
  | '*' -> add_token next STAR
  | '!' -> add_two_char_token next BANG_EQUAL BANG
  | '=' -> add_two_char_token next EQUAL_EQUAL EQUAL
  | '<' -> add_two_char_token next LESS_EQUAL LESS
  | '>' -> add_two_char_token next GREATER_EQUAL GREATER
  | '/' -> add_slash next
  | ' ' | '\r' | '\t' -> next
  | '\n' -> { next with line = next.line + 1 }
  | '"' -> handle_string next
  | '0' .. '9' -> handle_digit next
  | alpha when is_alpha alpha -> handle_identifier next
  | _ ->
      error state.line "Unexpected character.";
      next

(** Scan tokens. We reverse the list at the end; we are prepending because
    it's more efficient. *)
let scan_tokens source =
  let state = default_state source in
  let rec scan state =
    match is_at_end state with
    | false -> { state with start = state.current } |> scan_token |> scan
    | true -> add_token state EOF
  in
  List.rev (scan state).tokens
