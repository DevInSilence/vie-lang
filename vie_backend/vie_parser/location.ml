(* location.ml *)
type t = { start_idx : int; end_idx : int; column : int; line : int } [@@deriving show]

let new_location (start_idx : int) (end_idx : int) (column : int) (line : int) :
    t =
  { start_idx; end_idx; column; line }