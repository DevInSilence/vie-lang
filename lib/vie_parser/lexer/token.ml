type t =
  | Number of (int * Location.t)
  | Decimal of (float * Location.t)
  | Iden of (string * Location.t)
  | Operator of (string * Location.t)
  | Assign of Location.t
  | EOF of Location.t

let format (tok : t) : string =
  match tok with
  | Number (num, loc) ->
      Printf.sprintf "Tok { type: Number, value: %d, loc: %s }" num
        (Location.format loc)
  | Decimal (num, loc) ->
      Printf.sprintf "Tok { type: Decimal, value: %f, loc: %s }" num
        (Location.format loc)
  | Iden (iden, loc) ->
      Printf.sprintf "Tok { type: Iden, value: %s, loc: %s }" iden
        (Location.format loc)
  | Operator (op, loc) ->
      Printf.sprintf "Tok { type: Operator, value: %s, loc: %s }" op
        (Location.format loc)
  | Assign loc ->
      Printf.sprintf "Tok { type: Assign, loc: %s}" (Location.format loc)
  | EOF loc -> Printf.sprintf "Tok { type: EOF, loc: %s}" (Location.format loc)
