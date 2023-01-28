open Lexer

let run source = scan_tokens source |> Print.Token.print_out

let run_prompt () =
  while true do
    print_string "> ";
    flush stdout;
    run (read_line ())
  done

let run_file path =
  let ic = open_in path in
  let len = in_channel_length ic in
  really_input_string ic len |> run

let main (args : string array) =
  match Array.length args with
  | 1 -> run_prompt ()
  | 2 -> run_file args.(1)
  | _ ->
      print_endline "Usage: olox [script]";
      exit 64
