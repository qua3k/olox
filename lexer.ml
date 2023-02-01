open Error

type token = { t : Token.t; line : int }

type state = {
  source : string;
  tokens : token list;
  start : int;
  current : int;
  line : int;
}

let default source =
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

(** [advance_past state f] Advance state to end as long as the predicate
    is true, setting current to the first char that doesn't meet the
    predicate. *)
let rec advance_past state f =
  match get_current_char state |> f with
  | true -> advance_past (advance state) f
  | false -> state

let add_slash next =
  match get_current_char next with
  | '/' -> advance_past next (fun c -> c <> '\n')
  (* | '*' -> If we allow nested comments we need to keep count of how deep
     we're nesting. *)
  | _ -> add_token next Slash

let is_digit = function '0' .. '9' -> true | _ -> false

let is_alpha = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false

let is_alpha_numeric ch = is_alpha ch || is_digit ch

let substring state =
  try
    let start = state.start in
    Some (String.sub state.source start (state.current - start))
  with _ -> None

let float_substring state =
  match Option.bind (substring state) Float.of_string_opt with
  | Some f -> add_token state (Number f)
  | None -> state

let handle_string next =
  let state = advance_past next (fun c -> c <> '"') in
  match is_at_end state with
  | true -> (Error (error next.line "Unterminated string."), next)
  | false -> (
      let after = advance state in
      ( Ok,
        match substring after with
        | Some s ->
            add_token after (String s) (* advances past last quote *)
        | None -> next ))

let handle_digit state =
  let next = advance_past state is_digit in
  match get_current_char next with
  | '.' -> (
      let after = advance next in
      match after |> get_current_char |> is_digit with
      | true -> advance_past after is_digit |> float_substring
      | false -> float_substring next)
  | _ -> float_substring next

let handle_identifier state =
  let next = advance_past state is_alpha_numeric in
  match substring next with
  | Some s ->
      let open Token in
      let token_type =
        match s with
        | "and" -> And
        | "class" -> Class
        | "else" -> Else
        | "false" -> False
        | "fun" -> Fun
        | "for" -> For
        | "if" -> If
        | "nil" -> Nil
        | "or" -> Or
        | "print" -> Print
        | "return" -> Return
        | "super" -> Super
        | "this" -> This
        | "true" -> True
        | "var" -> Var
        | "while" -> While
        | _ -> Identifier
      in
      add_token next token_type
  | None -> next

let scan_token state =
  let next = advance state in
  match get_current_char state with
  | '(' -> (Ok, add_token next Left_paren)
  | ')' -> (Ok, add_token next Right_paren)
  | '{' -> (Ok, add_token next Left_brace)
  | '}' -> (Ok, add_token next Right_paren)
  | ',' -> (Ok, add_token next Comma)
  | '.' -> (Ok, add_token next Dot)
  | '-' -> (Ok, add_token next Minus)
  | '+' -> (Ok, add_token next Plus)
  | ';' -> (Ok, add_token next Semicolon)
  | '*' -> (Ok, add_token next Star)
  | '!' -> (Ok, add_two_char_token next Bang_equal Bang)
  | '=' -> (Ok, add_two_char_token next Equal_equal Equal)
  | '<' -> (Ok, add_two_char_token next Less_equal Less)
  | '>' -> (Ok, add_two_char_token next Greater_equal Greater)
  | '/' -> (Ok, add_slash next)
  | ' ' | '\r' | '\t' -> (Ok, next)
  | '\n' -> (Ok, { next with line = next.line + 1 })
  | '"' -> handle_string next
  | '0' .. '9' -> (Ok, handle_digit next)
  | alpha when is_alpha alpha -> (Ok, handle_identifier next)
  | _ -> (Error (error state.line "Unexpected character."), next)

let empty = function [] -> true | _ -> false

(** Scan tokens. We reverse the list at the end; we are prepending because
    it's more efficient. This should aim to be purely functional, so no
    mutable state. *)
let scan_tokens source =
  let rec scan (r, s) errors =
    let es = match r with Ok -> errors | Error e -> e :: errors in
    match (is_at_end s, empty es) with
    | false, _ -> scan ({ s with start = s.current } |> scan_token) es
    | true, true -> Result.Ok (add_token s Eof)
    | true, false -> Error (List.rev es)
  in
  let* r = scan (Ok, default source) [] in
  Ok (List.rev r.tokens)
