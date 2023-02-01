open Lexer

let rec perrors = function
  | hd :: tl ->
      prerr_endline hd;
      perrors tl
  | [] -> ()

let run source =
  match scan_tokens source with
  | Ok ts -> (
      match Parser.interpret ts with
      | Ok _ -> false
      | Error e ->
          prerr_endline e;
          true)
  | Error es ->
      perrors es;
      false

let run_prompt () =
  while true do
    print_string "> ";
    flush stdout;
    run (read_line ()) |> ignore
  done

let run_file path =
  let ic = open_in path in
  let len = in_channel_length ic in
  if really_input_string ic len |> run = true then exit 65

(* this should return a result *)

let main (args : string array) =
  match Array.length args with
  | 1 -> run_prompt ()
  | 2 -> run_file args.(1)
  | _ ->
      print_endline "Usage: olox [script]";
      exit 64
