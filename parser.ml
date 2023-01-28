(* Generate some ASTs. *)
open Lexer

let not_at_end token = token.t != EOF

let consume t = function
  | [] -> None
  | hd :: tl -> if hd.t = t then Some tl else None

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

let parse tokens = expression tokens |> fst

(** This module will be exception hell, because there is no way I'm
    writing pattern matches on Result. *)
module Eval = struct
  open Ast
  open Token

  let is_true = function BOOL false | NIL -> false | _ -> true

  let is_equal (a : literal) (b : literal) =
    match (a, b) with
    (* Deviate slightly to avoid converting "three" into 3. *)
    | STRING s, NUMBER n | NUMBER n, STRING s -> Float.to_string n = s
    | STRING a, STRING b -> a = b
    | NUMBER a, NUMBER b -> a = b (* Comparing floats??? *)
    | NIL, NIL -> true
    | _, NIL | NIL, _ -> false
    | _ -> false

  let rec evaluate = function
    | GROUPING { expr; _ } -> evaluate expr
    | BINARY { left; operator; right } -> binary left operator right
    | LITERAL l -> l
    | UNARY { operator; expr } -> unary operator expr
    | _ -> failwith "TODO"

  and unary op expr =
    let right = evaluate expr in
    match (op, right) with
    | MINUS, NUMBER n -> NUMBER (Float.neg n)
    | BANG, _ -> BOOL (is_true right |> not)
    | _ -> Invalid_argument "Must be a number." |> raise

  and binary left op right =
    let a = evaluate left and b = evaluate right in
    match (op, a, b) with
    | MINUS, NUMBER l, NUMBER r -> NUMBER (l -. r)
    | SLASH, NUMBER l, NUMBER r -> NUMBER (l /. r)
    | STAR, NUMBER l, NUMBER r -> NUMBER (l *. r)
    | PLUS, NUMBER l, NUMBER r -> NUMBER (l +. r)
    | PLUS, STRING l, STRING r -> STRING (l ^ r)
    | GREATER, NUMBER l, NUMBER r -> BOOL (l > r)
    | GREATER_EQUAL, NUMBER l, NUMBER r -> BOOL (l >= r)
    | LESS, NUMBER l, NUMBER r -> BOOL (l < r)
    | LESS_EQUAL, NUMBER l, NUMBER r -> BOOL (l <= r)
    | BANG_EQUAL, _, _ -> BOOL (is_equal a b |> not)
    | EQUAL, _, _ -> BOOL (is_equal a b)
    | _ -> Invalid_argument "Arguments must be of the same type." |> raise

  let interpret expr = Result.(try Ok (evaluate expr) with e -> Error e)
end
