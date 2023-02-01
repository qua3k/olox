let ( let* ) = Result.bind

(* Custom result type *)

type 'a result = Ok | Error of 'a

let report line where message =
  Printf.sprintf "[line %s] Error %s : %s" line where message

let error line message = report (string_of_int line) "" message
