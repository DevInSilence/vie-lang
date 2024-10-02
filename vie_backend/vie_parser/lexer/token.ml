type t =
  | Number of (int * Location.t)
  | Decimal of (float * Location.t)
  | Iden of (string * Location.t)
  | Operator of (string * Location.t)
  | Assign of Location.t
  | EOF of Location.t
  [@@deriving show]