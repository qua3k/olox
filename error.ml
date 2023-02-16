let ( let* ) = Result.bind

let report line where message =
  Printf.sprintf "[line %s] Error %s : %s" line where message

let error line message = report (string_of_int line) "" message
