(* Generate some ASTs. *)
open Lexer

let not_at_end token = token.t <> Eof

let consume t = function
  | hd :: tl -> if hd.t = t then Some tl else None
  | [] -> None

(* Should we return an empty list or None? The former should be more
   expensive. *)
let match_tokens xs ts =
  match xs with
  | hd :: tl ->
      if not_at_end hd && List.mem hd.t ts then Some (hd.t, tl) else None
  | [] -> None

let rec match_right (hd, tl) types f =
  match match_tokens tl types with
  | Some tuple -> match_right tuple types f
  | None ->
      let right, xs = f tl in
      (hd, right, xs)

let rec synchronize = function
  | a :: b :: tl -> (
      let xs = b :: tl in
      match (a.t, b.t) with
      | Eof, _
      | Semicolon, _
      | _, Class
      | _, Fun
      | _, Var
      | _, For
      | _, If
      | _, While
      | _, Print
      | _, Return ->
          xs
      | _ -> synchronize xs)
  | _ as l -> l

module Expression = struct
  let match_tree ts types f =
    let ((left, tl) as tuple) = f ts in
    match match_tokens tl types with
    | Some t ->
        let operator, right, xs = match_right t types f in
        (Ast.Binary { left; operator; right }, xs)
    | None -> tuple

  (* Should we rewrite all the prototypes to return a Result? It wouldn't
     be that much work since we've essentially consolodated into one
     function. *)
  let rec primary = function
    | hd :: tl -> (
        match hd.t with
        | False -> (Ast.Literal (Bool false), tl)
        | True -> (Ast.Literal (Bool true), tl)
        | Nil -> (Ast.Literal Nil, tl)
        | Number n -> (Ast.Literal (Number n), tl)
        | String s -> (Ast.Literal (String s), tl)
        | Left_paren -> (
            let expr, xs = expression tl in
            match consume Right_paren xs with
            | Some t -> (Ast.Grouping expr, t)
            | None -> failwith "Expect ')' after expression."
            (* consume, stop iterating and fail *))
        | _ -> failwith "Expect expression.")
    | [] -> failwith "Input expected."

  and unary tokens =
    match match_tokens tokens [ Bang; Minus ] with
    | Some (operator, tl) ->
        let expr, xs = unary tl in
        (Ast.Unary { operator; expr }, xs)
    | _ -> primary tokens

  and factor tokens = match_tree tokens [ Slash; Star ] unary
  and term tokens = match_tree tokens [ Minus; Plus ] factor

  and comparison tokens =
    match_tree tokens [ Greater; Greater_equal; Less; Less_equal ] term

  and equality tokens =
    match_tree tokens [ Bang_equal; Equal_equal ] comparison

  and expression tokens = equality tokens
end

(** This module will be exception hell, because there is no way I'm
    writing pattern matches on Result. *)
module Eval = struct
  open Ast
  open Token

  let is_true = function Bool false | Nil -> false | _ -> true

  let is_equal (a : literal) (b : literal) =
    match (a, b) with
    | String a, String b -> a = b
    | Number a, Number b -> a = b (* Comparing floats??? *)
    | Nil, Nil -> true
    | _ -> false

  let rec evaluate = function
    | Grouping e -> evaluate e
    | Binary { left; operator; right } -> binary left operator right
    | Literal l -> l
    | Unary { operator; expr } -> unary operator expr
    | _ -> failwith "TODO"

  and unary op expr =
    let right = evaluate expr in
    match (op, right) with
    | Minus, Number n -> Number (-.n)
    | Bang, _ -> Bool (is_true right |> not)
    | _ -> Invalid_argument "Must be a number." |> raise

  and binary left op right =
    let a = evaluate left and b = evaluate right in
    match (op, a, b) with
    | Minus, Number l, Number r -> Number (l -. r)
    | Slash, Number l, Number r -> Number (l /. r)
    | Star, Number l, Number r -> Number (l *. r)
    | Plus, Number l, Number r -> Number (l +. r)
    | Plus, String l, String r -> String (l ^ r)
    | Greater, Number l, Number r -> Bool (l > r)
    | Greater_equal, Number l, Number r -> Bool (l >= r)
    | Less, Number l, Number r -> Bool (l < r)
    | Less_equal, Number l, Number r -> Bool (l <= r)
    | Bang_equal, _, _ -> Bool (is_equal a b |> not)
    | Equal_equal, _, _ -> Bool (is_equal a b)
    | _ -> Invalid_argument "Arguments must be of the same type." |> raise

  and print e = evaluate e |> Print.Ast.literal |> print_endline

  and statement = function
    | Expression e ->
        evaluate e |> ignore (* is this evaluated only for the errors? *)
    | Print e -> evaluate e |> Print.Ast.literal |> print_endline
end

module Statement = struct
  let print ts =
    let expr, xs = Expression.expression ts in
    match consume Semicolon xs with
    | Some t -> Ok (t, Ast.Print expr)
    | None -> Error "Expect ';' after value."

  let expression ts =
    let expr, xs = Expression.expression ts in
    match consume Semicolon xs with
    | Some t -> Ok (t, Ast.Expression expr)
    | None -> Error "Expect ';' after expression."

  let statement = function
    | hd :: tl as ts -> (
        match hd.t with Print -> print tl | _ -> expression ts)
    | [] -> Error "Input expected."

  let rec parse ts ss =
    match statement ts with
    | Ok ((hd :: _ as xs), s) -> (
        match hd.t with
        | Eof -> Ok (s :: ss |> List.rev)
        | _ -> parse xs (s :: ss))
    | Ok ([], s) -> Ok (s :: ss |> List.rev)
    | Error _ as e -> e
end

let interpret ts =
  match Statement.parse ts [] with
  | Ok ss ->
      let rec f = function
        | hd :: tl ->
            Eval.statement hd;
            f tl
        | [] -> Ok ()
      in
      f ss
  | Error _ as e -> e
