open Error

type token = { t : Token.t; line : int }
type state = { source : string; start : int; current : int; line : int }

let default source = { source; start = 0; current = 0; line = 1 }
let advance state = { state with current = state.current + 1 }
let is_at_end state = state.current >= String.length state.source

let get_current_char state =
  try state.source.[state.current] with _ -> '\x00'

let emit_two_char_token next a b =
  match get_current_char next with '=' -> a | _ -> b

(** [advance_past state f] Advance state to end as long as the predicate
    is true, setting current to the first char that doesn't meet the
    predicate. *)
let rec advance_past f state =
  let ch = get_current_char state in
  match ch with
  | '\x00' -> state
  | _ -> begin
      match f ch with
      | true -> advance_past f (advance state)
      | false -> state
    end

let emit_slash next =
  let open Either in
  match get_current_char next with
  | '/' -> Left (advance_past (fun c -> c <> '\n') next)
  (* | '*' -> If we allow nested comments we need to keep count of how deep
     we're nesting. *)
  | _ -> Right Token.Slash

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

(* should be infalliable *)
let emit_str state =
  let state' = advance_past (fun c -> c <> '"') state in
  match is_at_end state' with
  | true -> (Error (error state'.line "Unterminated string."), state')
  | false ->
      let state'' = advance state' in
      let str = substring state'' in
      (Ok (Token.String (Option.get str)), state'')

let bind_float s = Option.bind (substring s) Float.of_string_opt

let add_digit state =
  let advance_digit = advance_past is_digit in
  let state' = advance_digit state in
  match get_current_char state' with
  | '.' ->
      let state'' = advance state' in
      let s =
        match get_current_char state'' |> is_digit with
        | true -> advance_digit state''
        | false -> state'
      in
      bind_float s
  | _ -> bind_float state'

let t_of_string = function
  | "and" -> Token.And
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
  | _ as i -> Identifier i

let emit_ident state =
  let state' = advance_past is_alpha_numeric state in
  Option.bind (substring state') (fun s -> Some (t_of_string s, state'))

(* Only emit tokens or errors, otherwise we recurse *)
let rec emit_token state =
  let state' = { state with start = state.current } in
  let state'' = advance state' in
  let open Token in
  match is_at_end state' with
  | true -> (Ok Eof, state'')
  | false -> begin
      match get_current_char state' with
      | '(' -> (Ok Left_paren, state'')
      | ')' -> (Ok Right_paren, state'')
      | '{' -> (Ok Left_brace, state'')
      | '}' -> (Ok Right_brace, state'')
      | ',' -> (Ok Comma, state'')
      | '.' -> (Ok Dot, state'')
      | '-' -> (Ok Minus, state'')
      | '+' -> (Ok Plus, state'')
      | ';' -> (Ok Semicolon, state'')
      | '*' -> (Ok Star, state'')
      | '!' -> (Ok (emit_two_char_token state' Bang_equal Bang), state'')
      | '=' -> (Ok (emit_two_char_token state' Equal_equal Equal), state'')
      | '<' -> (Ok (emit_two_char_token state' Less_equal Less), state'')
      | '>' ->
          (Ok (emit_two_char_token state' Greater_equal Greater), state'')
      | '/' -> begin
          match emit_slash state'' with
          | Left s -> emit_token s
          | Right t -> (Ok t, state'')
        end
      | ' ' | '\r' | '\t' -> emit_token state''
      | '\n' -> emit_token { state'' with line = state''.line + 1 }
      | '"' -> emit_str state''
      | '0' .. '9' -> begin
          match add_digit state'' with
          | Some f -> (Ok (Number f), state'')
          | None -> emit_token state''
        end
      | alpha when is_alpha alpha -> begin
          match emit_ident state'' with
          | Some (t, s''') -> (Ok t, s''')
          | None -> emit_token state''
        end
      | _ -> (Error (error state'.line "Unexpected character."), state'')
    end

(** Scan tokens. We reverse the list at the end; we are prepending because
    it's more efficient. This should aim to be purely functional, so no
    mutable state. Return both errors and tokens and let the caller handle the
    output. *)
let scan_tokens source =
  let s = default source in
  let rec scan state ts es =
    match is_at_end state with
    | true -> begin
        match es with [] -> Ok (List.rev ts) | _ -> Error (List.rev es)
      end
    | false -> begin
        let result, state' = emit_token state in
        let scan' = scan state' in
        match result with
        | Ok t -> scan' ({ t; line = state'.line } :: ts) es
        | Error e -> scan' ts (e :: es)
      end
  in
  scan s [] []
