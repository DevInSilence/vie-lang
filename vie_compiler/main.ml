open Vie_parser
open Vie_parser.Parser
open Lexing

let () =
  let source = "123" in
  let lex_res = Lexer.lex source in
  match lex_res with
  | Ok tokens ->
      List.iter (fun t -> Printf.printf "%s\n" (Token.show t)) tokens;
      let program = Parser.program Lexer.lex lex_res in

  | Error errs ->
      List.iter (fun e -> Printf.printf "Error: %s\n" e) errs;
