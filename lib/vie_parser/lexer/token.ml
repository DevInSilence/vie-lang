type t = 
  | EOF of Location.t

let format (tok: t) : string =
  match tok with
  | EOF loc -> Printf.sprintf "Tok { loc: %s, type: EOF }" (Location.format loc)