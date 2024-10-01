type t =
  | Number of (int * Location.t)
  | Decimal of (float * Location.t)
  | EOF of Location.t

let format (tok : t) : string =
  match tok with
  | Number (num, loc) ->
      Printf.sprintf "Tok { type: Number, value: %d, loc: %s }" num
        (Location.format loc)
  | Decimal (num, loc) ->
      Printf.sprintf "Tok { type: Decimal, value: %f, loc: %s }" num
        (Location.format loc)
  | EOF loc -> Printf.sprintf "Tok { type: EOF, loc: %s}" (Location.format loc)
