open Lexer

let perrors = List.iter prerr_endline

(* Returns true if an errors occured *)
let run source =
  let rs = scan_tokens source in
  match Result.bind rs Parser.interpret with
  | Ok _ -> false
  | Error es ->
      perrors es;
      true

let run_prompt () =
  while true do
    print_string "> ";
    run (read_line ()) |> ignore
  done

let run_file path =
  let ic = open_in path in
  let len = in_channel_length ic in
  if really_input_string ic len |> run = true then exit 65

(* this should return a result *)
let main args =
  match Array.length args with
  | 1 -> run_prompt ()
  | 2 -> run_file args.(1)
  | _ ->
      print_endline "Usage: olox [script]";
      exit 64
