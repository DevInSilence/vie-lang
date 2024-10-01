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

let is_valid_operator c =
  let code = UChar.code c in
  code = UChar.code (UChar.of_char '+') || code = UChar.code (UChar.of_char '-') || code = UChar.code (UChar.of_char '*') || code = UChar.code (UChar.of_char '/') || code = UChar.code (UChar.of_char '%')

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

(* Handlers *)

let handle_whitespace source idx_ref line_ref column_ref =
  match peek_char source !idx_ref with
  | Some c when is_whitespace c ->
      update_pos line_ref column_ref c;
      idx_ref := !idx_ref + 1;
      true
  | _ -> false

let handle_number source idx_ref line_ref column_ref tokens_ref errs_ref =
  match peek_char source !idx_ref with
  | Some c when is_valid_number_start c || UChar.code c = UChar.code (UChar.of_char '.') ->
      let start_idx = !idx_ref in
      (match lex_number source start_idx idx_ref line_ref column_ref with
      | Result.Ok token -> append_token tokens_ref token
      | Result.Error err -> append_error errs_ref err);
      true
  | _ -> false
  
  
let handle_identifier source idx_ref line_ref column_ref tokens_ref errs_ref =
  match peek_char source !idx_ref with
  | Some c when is_valid_identifier_start c ->
      let start_idx = !idx_ref in
      (match lex_identifiers source start_idx idx_ref line_ref column_ref with
      | Result.Ok token -> append_token tokens_ref token
      | Result.Error err -> append_error errs_ref err);
      true
  | _ -> false

let handle_unexpected_char source idx_ref line_ref column_ref errs_ref =
  match peek_char source !idx_ref with
  | Some c ->
      errs_ref := Format.sprintf "Unexpected character '%s' at line %d, column %d"
                                  (Vie_common.Unicode.uchar_to_string c)
                                  !line_ref !column_ref :: !errs_ref;
      idx_ref := !idx_ref + 1;
      true
  | None -> false

let handle_operator source idx_ref line_ref column_ref tokens_ref =
  match peek_char source !idx_ref with
  | Some c when is_valid_operator c ->
      let start_idx = !idx_ref in
      let loc = Location.new_location start_idx (!idx_ref + 1) !column_ref !line_ref in
      append_token tokens_ref (Token.Operator (Vie_common.Unicode.uchar_to_string c, loc));
      update_pos line_ref column_ref c;
      idx_ref := !idx_ref + 1;
      true
  | _ -> false

let handle_assign source idx_ref line_ref column_ref tokens_ref =
  match peek_char source !idx_ref with
  | Some c when UChar.code c = UChar.code (UChar.of_char '=') ->
      let start_idx = !idx_ref in
      let loc = Location.new_location start_idx (!idx_ref + 1) !column_ref !line_ref in
      append_token tokens_ref (Token.Assign loc);
      update_pos line_ref column_ref c;
      idx_ref := !idx_ref + 1;
      true
  | _ -> false

(* Main lexer function *)
  

let rec lex_token source idx_ref line_ref column_ref tokens_ref errs_ref =
  if !idx_ref >= UTF8.length source then
    append_token tokens_ref (Token.EOF (Location.new_location !idx_ref !idx_ref !column_ref !line_ref))
  else if handle_whitespace source idx_ref line_ref column_ref then
    lex_token source idx_ref line_ref column_ref tokens_ref errs_ref
  else if handle_number source idx_ref line_ref column_ref tokens_ref errs_ref then
    lex_token source idx_ref line_ref column_ref tokens_ref errs_ref
  else if handle_operator source idx_ref line_ref column_ref tokens_ref then
    lex_token source idx_ref line_ref column_ref tokens_ref errs_ref
  else if handle_assign source idx_ref line_ref column_ref tokens_ref then
    lex_token source idx_ref line_ref column_ref tokens_ref errs_ref
  else if handle_identifier source idx_ref line_ref column_ref tokens_ref errs_ref then
    lex_token source idx_ref line_ref column_ref tokens_ref errs_ref
  else if handle_unexpected_char source idx_ref line_ref column_ref errs_ref then
    lex_token source idx_ref line_ref column_ref tokens_ref errs_ref
  else ()
  

(* Main lexer function *)

let lex source : (Token.t list, string list) Result.t =
  let idx = ref 0 in
  let line = ref 1 in
  let column = ref 1 in
  let tokens = ref [] in
  let errs = ref [] in

  lex_token source idx line column tokens errs;

  if List.length !errs > 0 then Result.Error (List.rev !errs)
  else Result.Ok (List.rev !tokens)
