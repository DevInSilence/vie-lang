open Vie_common
module Location = Vie_lexer.Location
module Token = Vie_lexer.Token
open Camomile
module UTF8 = Camomile.UTF8

(* Utility functions *)

let update_pos line_ref column_ref char =
  if UChar.code char = (UChar.code (UChar.of_char '\n')) then (
    line_ref := !line_ref + 1;
    column_ref := 1
  ) else
    column_ref := !column_ref + 1

let append_token tokens_ref tok = tokens_ref := tok :: !tokens_ref
let append_error errs_ref err = errs_ref := err :: !errs_ref

let peek_char source idx =
  if idx >= UTF8.length source then None
  else Some (UTF8.get source idx)

let collect source start_idx idx : string =
  Vie_common.Unicode.utf8_sub source start_idx (idx - start_idx)

let classify_identifier iden loc: Token.t =
  match iden with
  | _ -> Token.Iden (iden, loc)

let is_valid_number_start c =
  let code = UChar.code c in
  code >= UChar.code (UChar.of_char '0') && code <= UChar.code (UChar.of_char '9')

let is_valid_identifier_start c =
  let code = UChar.code c in
  (code >= UChar.code (UChar.of_char 'a') && code <= UChar.code (UChar.of_char 'z')) ||
  (code >= UChar.code (UChar.of_char 'A') && code <= UChar.code (UChar.of_char 'Z')) || 
  code = UChar.code (UChar.of_char '_')

let is_valid_identifier_continuation c =
  is_valid_identifier_start c || is_valid_number_start c

let is_whitespace c =
  let code = UChar.code c in
  code = UChar.code (UChar.of_char ' ') || code = UChar.code (UChar.of_char '\n') || code = UChar.code (UChar.of_char '\t')

(* Lexer functions *)

let lex_number source start_idx idx_ref line_ref column_ref : (Token.t, string) Result.t =
  let is_float = ref false in
  let rec collect_number () =
    match peek_char source !idx_ref with
    | Some c when UChar.code c = UChar.code (UChar.of_char '.') && not !is_float ->
        update_pos line_ref column_ref c;
        idx_ref := !idx_ref + 1;
        is_float := true;
        collect_number ()
    | Some c when is_valid_number_start c ->
        update_pos line_ref column_ref c;
        idx_ref := !idx_ref + 1;
        collect_number ()
        | _ -> 
          let num_str = collect source start_idx !idx_ref in
          let loc = Location.new_location start_idx !idx_ref !column_ref !line_ref in
          if !is_float then
            try Result.Ok (Token.Decimal (float_of_string num_str, loc))
            with Failure _ -> Result.Error (Printf.sprintf "Invalid float format: '%s'" num_str)
          else
            try Result.Ok (Token.Number (int_of_string num_str, loc))
            with Failure _ -> Result.Error (Printf.sprintf "Invalid integer format: '%s'" num_str)
          in  
  collect_number ()

let lex_identifiers source start_idx idx_ref line_ref column_ref : (Token.t, string) Result.t =
  let rec collect_identifier () =
    match peek_char source !idx_ref with
    | Some c when is_valid_identifier_continuation c ->
        update_pos line_ref column_ref c;
        idx_ref := !idx_ref + 1;
        collect_identifier ()
    | _ ->
        let iden = collect source start_idx !idx_ref in
        let loc = Location.new_location start_idx !idx_ref !column_ref !line_ref in
        Result.Ok (classify_identifier iden loc)
  in
  collect_identifier ()

(* Main lexer function *)

let lex source : (Token.t list, string list) Result.t =
  let len = String.length source in
  let idx = ref 0 in
  let line = ref 1 in
  let column = ref 1 in
  let tokens = ref [] in
  let errs = ref [] in

  let rec lex_token () =
    if !idx >= len then
      append_token tokens (Token.EOF (Location.new_location !idx !idx !column !line))
    else
      match peek_char source !idx with
      | Some c when is_whitespace c ->
          update_pos line column c;
          idx := !idx + 1;
          lex_token ()
      | Some c when is_valid_number_start c ->
          let start_idx = !idx in
          (match lex_number source start_idx idx line column with
          | Result.Ok token -> append_token tokens token
          | Result.Error err -> append_error errs err);
          lex_token ()
      | Some c when UChar.code c = UChar.code (UChar.of_char '.') ->
          (match peek_char source (!idx + 1) with
          | Some next_c when is_valid_number_start next_c ->
              let start_idx = !idx in
              (match lex_number source start_idx idx line column with
              | Result.Ok token -> append_token tokens token
              | Result.Error err -> append_error errs err);
              lex_token ()
          | _ ->
              errs := Format.sprintf "Unexpected character '.' at line %d, column %d" !line !column :: !errs;
              idx := !idx + 1;
              lex_token ())
      | Some c when is_valid_identifier_start c ->
          let start_idx = !idx in
          (match lex_identifiers source start_idx idx line column with
          | Result.Ok token -> append_token tokens token
          | Result.Error err -> append_error errs err);
          lex_token ()
      | Some c ->
          errs := Format.sprintf "Unexpected character '%s' at line %d, column %d"
                                  (Vie_common.Unicode.uchar_to_string c)
                                  !line !column :: !errs;
          idx := !idx + 1;
          lex_token ()
      | None -> ()
  in

  lex_token ();
  if List.length !errs > 0 then Result.Error (List.rev !errs)
  else Result.Ok (List.rev !tokens)
