(* lexer.ml *)
module Location = Vie_lexer.Location
module Token = Vie_lexer.Token

let update_pos line_ref column_ref char = 
  if char = '\n' then (
    line_ref := !line_ref + 1;
    column_ref := 1
  ) else (
    column_ref := !column_ref + 1
  )

let append_token tokens_ref tok = 
  tokens_ref := tok :: !tokens_ref

let lex source : (Token.t list, string list) Vie_common.Result.t =
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
      | _ -> (
        errs := "Unexpected character" :: !errs;
        idx := !idx + 1;
        lex_token ()
      )
    )
  in
  lex_token ();

  if List.length !errs > 0 then Vie_common.Result.Error (List.rev !errs)
  else Vie_common.Result.Ok (List.rev !tokens)