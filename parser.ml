(* Generate some ASTs. *)
open Lexer

let token_error t message =
  let where = Printf.sprintf " at '%s'" (Token.to_string t.t) in
  Error.report (string_of_int t.line) where message |> prerr_endline

let consume t = function
  | hd :: tl -> if hd.t = t then Some tl else None
  | [] -> None

(* Should we return an empty list or None? The former should be more
   expensive. *)
let match_tokens xs ts =
  match xs with
  | hd :: tl -> if List.mem hd.t ts then Some (hd.t, tl) else None
  | [] -> None

(* recur *)
let rec match_right (hd, tl) types f =
  match match_tokens tl types with
  | Some tuple -> match_right tuple types f
  | None ->
      let right, xs = f tl in
      (hd, right, xs)

let rec synchronize = function
  | a :: b :: tl -> begin
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
      | _ -> synchronize xs
    end
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
    | hd :: tl -> begin
        match hd.t with
        | False -> (Ast.Literal (Bool false), tl)
        | True -> (Literal (Bool true), tl)
        | Nil -> (Literal Nil, tl)
        | Number n -> (Literal (Number n), tl)
        | String s -> (Literal (String s), tl)
        | Left_paren -> begin
            let expr, xs = expression tl in
            match consume Right_paren xs with
            | Some t -> (Grouping expr, t)
            | None -> failwith "Expect ')' after expression."
            (* consume, stop iterating and fail *)
          end
        | Identifier i -> (Variable i, tl)
        | _ -> failwith "Expect expression."
      end
    | [] -> failwith "Input expected."

  and unary tokens =
    match match_tokens tokens [ Bang; Minus ] with
    | Some (operator, tl) ->
        let expr, xs = unary tl in
        (Ast.Unary { operator; expr }, xs)
    | None -> primary tokens

  and factor tokens = match_tree tokens [ Slash; Star ] unary
  and term tokens = match_tree tokens [ Minus; Plus ] factor

  and comparison tokens =
    match_tree tokens [ Greater; Greater_equal; Less; Less_equal ] term

  and equality tokens =
    match_tree tokens [ Bang_equal; Equal_equal ] comparison

  and assignment tokens =
    let expr, ts = equality tokens in
    match consume Equal ts with
    | Some ts' -> begin
        match expr with
        | Variable name ->
            let e, ts'' = assignment ts' in
            (Ast.Assign { name; value = e }, ts'')
        | _ ->
            let hd = List.hd ts in
            token_error hd "Invalid assignment target";
            (expr, ts')
      end
    | None -> (expr, ts)

  and expression tokens = assignment tokens
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

  (* We thread the hashtable through the function calls; I wish there were a
     less verbose way to do this *)
  let rec evaluate tbls expr =
    match expr with
    | Grouping e -> evaluate tbls e
    | Binary { left; operator; right } -> binary tbls left operator right
    | Literal l -> l
    | Unary { operator; expr } -> unary tbls operator expr
    | Variable i -> variable tbls i
    | Assign { name; value } -> assign tbls name value

  and unary tbls op expr =
    let right = evaluate tbls expr in
    match (op, right) with
    | Minus, Number n -> Number (-.n)
    | Bang, _ -> Bool (is_true right |> not)
    | _ -> invalid_arg "Must be a number."

  and binary tbls left op right =
    let a = evaluate tbls left and b = evaluate tbls right in
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
    | _, String _, String _ -> invalid_arg "Operands must be numbers."
    | _ -> invalid_arg "Arguments must be of the same type."

  (* Recursively walk through enclosing scopes to find a defined variable. They
     use a singly-linked list in the book, which is exactly what an OCaml list
     is.

     TODO: use type system to enforce that variables can only be constructed
     with identifiers. *)
  and variable tbls init =
    match tbls with
    | hd :: tl -> begin
        match Hashtbl.find_opt hd init with
        | Some (Some i) -> i
        | Some None -> Nil
        | None -> variable tl init
      end
    | [] -> Printf.sprintf "Undefined variable '%s'." init |> invalid_arg

  and assign tbls name expr =
    match tbls with
    | hd :: tl -> begin
        match Hashtbl.mem hd name with
        | true ->
            let value = evaluate tbls expr in
            Hashtbl.replace hd name (Some value);
            value
        | false -> assign tl name expr
      end
    | [] -> Printf.sprintf "Undefined variable '%s'." name |> invalid_arg

  let print tbls e = evaluate tbls e |> Print.Ast.literal |> print_endline

  let rec statement tbls = function
    | Block ss -> execute_block tbls ss
    | Expression e ->
        evaluate tbls e
        |> ignore (* is this evaluated only for the errors? *)
    | Print e -> print tbls e
    | Var { name; init } ->
        let l = Option.bind init (fun e -> Some (evaluate tbls e)) in
        (* TODO: pattern match this away *)
        let tbl = List.hd tbls in
        Hashtbl.add tbl name l

  and execute_block tbls ss =
    let tbls' = Hashtbl.create 10 :: tbls in
    List.iter (statement tbls') ss
end

module Statement = struct
  let rec statement = function
    | { t = Print; _ } :: tl -> print tl
    | { t = Left_brace; _ } :: tl -> block tl []
    | _ :: _ as ts -> expression ts
    | [] -> Error "Input expected."

  and print ts =
    let e, xs = Expression.expression ts in
    match consume Semicolon xs with
    | Some t -> Ok (t, Ast.Print e)
    | None -> Error "Expect ';' after value."

  and block ts ss =
    match ts with
    | { t = Right_brace | Eof; _ } :: _ -> begin
        match consume Right_brace ts with
        | Some ts' -> Ok (ts', Ast.Block (List.rev ss))
        | None -> Error "Expect '}' after block."
      end
    | _ -> begin
        match declaration ts with
        | Ok s, ts' -> block ts' (s :: ss)
        | Error e, _ -> Error e
      end

  and expression ts =
    let e, xs = Expression.expression ts in
    match consume Semicolon xs with
    | Some t -> Ok (t, Expression e)
    | None ->
        Print.Token.print_out xs;
        Error "Expect ';' after expression."

  and var ts =
    match ts with
    | { t = Identifier i; _ } :: tl -> begin
        let init, ts'' =
          match consume Equal tl with
          | Some ts''' ->
              let e, ts'''' = Expression.expression ts''' in
              (Some e, ts'''')
          | None -> (None, tl)
        in
        match consume Semicolon ts'' with
        | Some ts''' -> Ok (ts''', Ast.Var { name = i; init })
        | None -> Error "Expect ';' after variable declaration."
      end
    | _ -> Error "Expect variable name."

  and var_statement ts =
    match consume Var ts with Some ts' -> var ts' | None -> statement ts

  and declaration ts =
    match var_statement ts with
    | Ok (ts'', s) -> (Ok s, ts'')
    | Error _ as e -> (e, synchronize ts)

  let parse_tokens ts =
    let rec parse ts ss es =
      match ts with
      | { t = Eof; _ } :: _ | [] -> begin
          match es with [] -> Ok (List.rev ss) | _ -> Error (List.rev es)
        end
      | _ -> begin
          let r, ts' = declaration ts in
          let parse' = parse ts' in
          match r with
          | Ok s -> parse' (s :: ss) es
          | Error e -> parse' ss (e :: es)
        end
    in
    parse ts [] []
end

let interpret ts =
  let inter ss =
    let tbls = [ Hashtbl.create 10 ] in
    Ok (List.iter (Eval.statement tbls) ss)
  in
  Result.bind (Statement.parse_tokens ts) inter