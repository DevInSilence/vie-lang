open Vie_common
open Camomile
module UTF8 = Camomile.UTF8

type t = {
  source : string;
  mutable idx : int;
  mutable line : int;
  mutable column : int;
  mutable tokens : Token.t list;
  mutable errs : string list;
}

(* Utility functions *)
let update_pos ctx char =
  if UChar.code char = UChar.code (UChar.of_char '\n') then (
    ctx.line <- ctx.line + 1;
    ctx.column <- 1)
  else ctx.column <- ctx.column + 1

let append_token ctx tok = ctx.tokens <- tok :: ctx.tokens
let append_error ctx err = ctx.errs <- err :: ctx.errs

let peek_char ctx =
  if ctx.idx >= UTF8.length ctx.source then None else Some (UTF8.get ctx.source ctx.idx)

let collect ctx start_idx : string =
  Vie_common.Unicode.utf8_sub ctx.source start_idx (ctx.idx - start_idx)

let classify_identifier iden loc : Token.t =
  match iden with _ -> Token.Iden (iden, loc)

let is_valid_number_start c =
  let code = UChar.code c in
  code >= UChar.code (UChar.of_char '0') && code <= UChar.code (UChar.of_char '9')

let is_valid_identifier_start c =
  let code = UChar.code c in
  code >= UChar.code (UChar.of_char 'a') && code <= UChar.code (UChar.of_char 'z')
  || code >= UChar.code (UChar.of_char 'A') && code <= UChar.code (UChar.of_char 'Z')
  || code = UChar.code (UChar.of_char '_')

let is_valid_identifier_continuation c =
  is_valid_identifier_start c || is_valid_number_start c

let is_whitespace c =
  let code = UChar.code c in
  code = UChar.code (UChar.of_char ' ') || code = UChar.code (UChar.of_char '\n') || code = UChar.code (UChar.of_char '\t')

let is_valid_operator c =
  let code = UChar.code c in
  code = UChar.code (UChar.of_char '+') || code = UChar.code (UChar.of_char '-')
  || code = UChar.code (UChar.of_char '*') || code = UChar.code (UChar.of_char '/')
  || code = UChar.code (UChar.of_char '%')

(* Lexer functions *)

let lex_number ctx start_idx : (Token.t, string) Result.t =
  let is_float = ref false in
  let rec collect_number () =
    match peek_char ctx with
    | Some c when UChar.code c = UChar.code (UChar.of_char '.') && not !is_float ->
        update_pos ctx c;
        ctx.idx <- ctx.idx + 1;
        is_float := true;
        collect_number ()
    | Some c when is_valid_number_start c ->
        update_pos ctx c;
        ctx.idx <- ctx.idx + 1;
        collect_number ()
    | _ ->
        let num_str = collect ctx start_idx in
        let loc = Location.new_location start_idx ctx.idx ctx.column ctx.line in
        if !is_float then
          try Result.Ok (Token.Decimal (float_of_string num_str, loc))
          with Failure _ -> Result.Error (Printf.sprintf "Invalid float format: '%s'" num_str)
        else
          try Result.Ok (Token.Number (int_of_string num_str, loc))
          with Failure _ -> Result.Error (Printf.sprintf "Invalid integer format: '%s'" num_str)
  in
  collect_number ()

let lex_identifiers ctx start_idx : (Token.t, string) Result.t =
  let rec collect_identifier () =
    match peek_char ctx with
    | Some c when is_valid_identifier_continuation c ->
        update_pos ctx c;
        ctx.idx <- ctx.idx + 1;
        collect_identifier ()
    | _ ->
        let iden = collect ctx start_idx in
        let loc = Location.new_location start_idx ctx.idx ctx.column ctx.line in
        Result.Ok (classify_identifier iden loc)
  in
  collect_identifier ()

(* Checkers *)
let check_whitespace ctx =
  match peek_char ctx with
  | Some c when is_whitespace c -> true
  | _ -> false

let check_number ctx =
  match peek_char ctx with
  | Some c when is_valid_number_start c || UChar.code c = UChar.code (UChar.of_char '.') -> true
  | _ -> false

let check_identifier ctx =
  match peek_char ctx with
  | Some c when is_valid_identifier_start c -> true
  | _ -> false

let check_operator ctx =
  match peek_char ctx with
  | Some c when is_valid_operator c -> true
  | _ -> false

let check_assign ctx =
  match peek_char ctx with
  | Some c when UChar.code c = UChar.code (UChar.of_char '=') -> true
  | _ -> false

let check_unexpected_char ctx =
  match peek_char ctx with Some _ -> true | None -> false

(* Handlers *)
let handle_whitespace ctx =
  let c = Option.get (peek_char ctx) in
  update_pos ctx c;
  ctx.idx <- ctx.idx + 1

let handle_number ctx =
  let start_idx = ctx.idx in
  match lex_number ctx start_idx with
  | Result.Ok token -> append_token ctx token
  | Result.Error err -> append_error ctx err

let handle_identifier ctx =
  let start_idx = ctx.idx in
  match lex_identifiers ctx start_idx with
  | Result.Ok token -> append_token ctx token
  | Result.Error err -> append_error ctx err

let handle_operator ctx =
  let start_idx = ctx.idx in
  let loc = Location.new_location start_idx (ctx.idx + 1) ctx.column ctx.line in
  append_token ctx (Token.Operator (Vie_common.Unicode.uchar_to_string (Option.get (peek_char ctx)), loc));
  update_pos ctx (Option.get (peek_char ctx));
  ctx.idx <- ctx.idx + 1

let handle_assign ctx =
  let start_idx = ctx.idx in
  let loc = Location.new_location start_idx (ctx.idx + 1) ctx.column ctx.line in
  append_token ctx (Token.Assign loc);
  update_pos ctx (Option.get (peek_char ctx));
  ctx.idx <- ctx.idx + 1

let handle_unexpected_char ctx =
  let c = Option.get (peek_char ctx) in
  ctx.errs <- Format.sprintf "Unexpected character '%s' at line %d, column %d"
    (Vie_common.Unicode.uchar_to_string c) ctx.line ctx.column :: ctx.errs;
  ctx.idx <- ctx.idx + 1

(* Main lexer function with pattern matching *)
let rec lex_token ctx =
  let next_char = peek_char ctx in
  match next_char with
  | None -> 
      append_token ctx (Token.EOF (Location.new_location ctx.idx ctx.idx ctx.column ctx.line))
  | Some c when is_whitespace c -> 
      handle_whitespace ctx;
      lex_token ctx
  | Some c when is_valid_number_start c || UChar.code c = UChar.code (UChar.of_char '.') -> 
      handle_number ctx;
      lex_token ctx
  | Some c when is_valid_operator c -> 
      handle_operator ctx;
      lex_token ctx
  | Some c when UChar.code c = UChar.code (UChar.of_char '=') -> 
      handle_assign ctx;
      lex_token ctx
  | Some c when is_valid_identifier_start c -> 
      handle_identifier ctx;
      lex_token ctx
  | Some _ -> 
      handle_unexpected_char ctx;
      lex_token ctx

(* Main lexer function *)

let lex source : (Token.t list, string list) Result.t =
  let ctx = { source; idx = 0; line = 1; column = 1; tokens = []; errs = [] } in

  lex_token ctx;

  if List.length ctx.errs > 0 then Result.Error (List.rev ctx.errs)
  else Result.Ok (List.rev ctx.tokens)
