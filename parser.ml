(* Generate some ASTs. *)
open Lexer

let not_at_end token = token.t != EOF

let consume t = function
  | [] -> None
  | hd :: tl -> if hd.t = t then Some tl else None

let match_token xs t =
  match xs with
  | hd :: tl ->
      if not_at_end hd && hd.t = t then Some (hd.t, tl) else None
  | _ -> None

(* Should we return an empty list or None? The former should be more
   expensive. *)
let match_tokens xs ts =
  match xs with
  | hd :: tl ->
      if not_at_end hd && List.mem hd.t ts then Some (hd.t, tl) else None
  | _ -> None

let rec match_right (hd, tl) types f =
  match match_tokens tl types with
  | Some tuple -> match_right tuple types f
  | None ->
      let right, xs = f tl in
      (hd, right, xs)

let rec match_tree ts types f =
  let ((left, tl) as tuple) = f ts in
  match match_tokens tl types with
  | Some t ->
      let operator, right, xs = match_right t types f in
      (Ast.BINARY { left; operator; right }, xs)
  | None -> tuple

and equality tokens =
  match_tree tokens [ BANG_EQUAL; EQUAL_EQUAL ] comparison

and expression tokens = equality tokens

and comparison tokens =
  match_tree tokens [ GREATER; GREATER_EQUAL; LESS; LESS_EQUAL ] term

and term tokens = match_tree tokens [ MINUS; PLUS ] factor
and factor tokens = match_tree tokens [ SLASH; STAR ] unary

and unary tokens =
  match match_tokens tokens [ BANG; MINUS ] with
  | Some (operator, tl) ->
      let expr, xs = unary tl in
      (Ast.UNARY { operator; expr }, xs)
  | _ -> primary tokens

(* Should we rewrite all the prototypes to return a Result? It wouldn't
   be that much work since we've essentially consolodated into one
   function. *)
and primary = function
  | hd :: tl -> (
      match hd.t with
      | FALSE -> (Ast.LITERAL (BOOL false), tl)
      | TRUE -> (Ast.LITERAL (BOOL true), tl)
      | NIL -> (Ast.LITERAL NIL, tl)
      | NUMBER n -> (Ast.LITERAL (NUMBER n), tl)
      | STRING s -> (Ast.LITERAL (STRING s), tl)
      | LEFT_PAREN -> (
          let expr, xs = expression tl in
          match consume RIGHT_PAREN xs with
          | Some t ->
              ( Ast.GROUPING
                  { left = LEFT_PAREN; expr; right = RIGHT_PAREN },
                t )
          | None -> failwith "Expect ')' after expression.")
      | _ -> failwith "Expect expression.")
  | _ -> failwith "Input expected."

let rec synchronize = function
  | a :: b :: tl -> (
      let xs = b :: tl in
      match (a.t, b.t) with
      | EOF, _
      | SEMICOLON, _
      | _, CLASS
      | _, FUN
      | _, VAR
      | _, FOR
      | _, IF
      | _, WHILE
      | _, PRINT
      | _, RETURN ->
          xs
      | _, _ -> synchronize xs)
  | _ as e -> e

let parse tokens = expression tokens
