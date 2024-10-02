(* location.ml *)
type t = { start_idx : int; end_idx : int; column : int; line : int }

let new_location (start_idx : int) (end_idx : int) (column : int) (line : int) :
    t =
  { start_idx; end_idx; column; line }

let format (loc : t) : string =
  Printf.sprintf "Loc { start: %d, end: %d, column: %d, line: %d }"
    loc.start_idx loc.end_idx loc.column loc.line
