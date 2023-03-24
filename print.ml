module Ast = struct
  open Ast
  open Token

  let rec expression = function
    | Binary b ->
        let left = expression b.left and right = expression b.right in
        Printf.sprintf "(%s %s %s)" (to_string b.operator) left right
    | Grouping g ->
        let expr = expression g in
        Printf.sprintf "(group %s)" expr
    | Literal Nil -> "nil"
    | Literal (Bool b) -> Bool.to_string b
    | Literal (Number f) -> Float.to_string f
    | Literal (String s) -> s
    | Unary u ->
        let str = to_string u.operator in
        Printf.sprintf "(%s %s)" str @@ expression u.expr
    | _ -> failwith "not implemented"

  let literal = function
    | Bool b -> Bool.to_string b
    | Number f -> begin
        let s = Float.to_string f in
        let l = String.length s in
        match s.[l - 1] = '.' with
        | true -> String.sub s 0 (l - 1)
        | false -> s
      end
    | String a -> a
    | Nil -> "nil"
end

module Token = struct
  open Lexer

  let print xs =
    let f token = Token.to_string token.t in
    List.map f xs

  let print_out xs = print xs |> List.iter print_endline
end
