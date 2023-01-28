module Ast = struct
  let rec print =
    let open Ast in
    let open Token in
    function
    | BINARY b ->
        let left, right = (print b.left, print b.right) in
        Printf.sprintf "(%s %s %s)" (to_string b.operator) left right
    | GROUPING g ->
        let expr = print g.expr in
        Printf.sprintf "%sgroup %s%s" (to_string g.left) expr
          (to_string g.right)
    | LITERAL NIL -> "nil"
    | LITERAL TRUE -> "true"
    | LITERAL FALSE -> "false"
    | LITERAL (NUMBER f) -> Float.to_string f
    | LITERAL (STRING s) -> s
    | UNARY u ->
        let expr = print u.expr in
        Printf.sprintf "(%s %s)" (to_string u.operator) expr
    | _ -> failwith "not implemented"
end

module Token = struct
  let print xs =
    let open Lexer in
    let f token = token.t |> Token.to_string in
      List.map f xs
  let print_out xs = print xs |> List.iter print_endline
end