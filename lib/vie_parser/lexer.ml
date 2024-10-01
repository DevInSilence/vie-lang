open Vie_common
module Location = Vie_lexer.Location
module Token = Vie_lexer.Token

(*Utility functions*)

let update_pos line_ref column_ref char = 
  if char = '\n' then (
    line_ref := !line_ref + 1;
    column_ref := 1
  ) else (
    column_ref := !column_ref + 1
  )

let append_token tokens_ref tok = 
  tokens_ref := tok :: !tokens_ref

let peek_char source idx = 
  if idx >= String.length source then None
  else Some source.[idx]

let collect source start_idx idx : string =
  String.sub source start_idx (idx - start_idx)

(*Lexers*)

let lex_number source start_idx idx_ref line_ref column_ref : (Token.t, string) Result.t =
  let is_float = ref false in
  let rec collect_number () =
    match peek_char source !idx_ref with
    | Some '.' when not !is_float -> 
      update_pos line_ref column_ref '.'; 
      idx_ref := !idx_ref + 1;
      is_float := true;
      collect_number ()
    | Some c when c >= '0' && c <= '9' -> 
      update_pos line_ref column_ref c;
      idx_ref := !idx_ref + 1;
      collect_number ()
    | _ ->
      let num_str = collect source start_idx !idx_ref in
      let loc = Location.new_location start_idx !idx_ref !column_ref !line_ref in
      if !is_float then
        try
          Result.Ok (Token.Decimal (float_of_string num_str, loc))
        with Failure _ -> Result.Error "Invalid float format"
      else
        try
          Result.Ok (Token.Number (int_of_string num_str, loc))
        with Failure _ -> Result.Error "Invalid integer format"
  in
  collect_number ()

let lex source : (Token.t list, string list) Result.t =
  let len = String.length source in

  let idx = ref 0 in     
  let line = ref 1 in    
  let column = ref 1 in  

  let tokens = ref ([] : Token.t list) in
  let errs = ref ([] : string list) in  

  let rec lex_token () =
    if !idx >= len then (
      append_token tokens (Token.EOF (Location.new_location !idx !idx !column !line))
    ) else (
      let char = source.[!idx] in
      match char with
      | ' ' | '\n' | '\t' -> (
        update_pos line column char;
        idx := !idx + 1;
        lex_token ()
      )
      | '0' .. '9' -> (
        let start_idx = !idx in
        (match lex_number source start_idx idx line column with
        | Result.Ok token -> append_token tokens token
        | Result.Error err -> errs := err :: !errs);
        lex_token ()
      )
      | _ -> (
        errs := "Unexpected character" :: !errs;
        idx := !idx + 1;
        lex_token ()
      )
    )
  in
  lex_token ();

  if List.length !errs > 0 then Result.Error (List.rev !errs)
  else Result.Ok (List.rev !tokens)
