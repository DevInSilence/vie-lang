open Vie_parser.Lexer
open Vie_common.Result  

let () =
  let tok_stream = lex " " in 
  match result_map (List.map Token.format) tok_stream with 
  | Ok formatted_tokens -> 
    List.iter print_endline formatted_tokens
  | Error errs ->
    List.iter print_endline errs
