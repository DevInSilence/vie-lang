type ('a, 'b) t = Ok of 'a | Error of 'b 

let result_ok = function Ok _ -> true | Error _ -> false
let result_error = function Ok _ -> false | Error _ -> true

let result_unwrap = function
  | Ok x -> x
  | Error _ -> failwith "Error: Tried to unwrap an error"

let result_unwrap_error = function
  | Ok _ -> failwith "Error: Tried to unwrap an ok"
  | Error e -> e

let result_map f = function Ok x -> Ok (f x) | Error e -> Error e
