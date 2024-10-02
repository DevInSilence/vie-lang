open Vie_parser.Lexer
open Vie_parser.Node

let () =
  let loc = Location.new_location 0 0 0 0 in
  let token = Token.Decimal (3.14, loc) in
  let value_expr = FloatExpr token in
  let op = PlusOp in
  let expr = BinaryExpr { left = value_expr; operator = op; right = value_expr } in
  let statement = Expression expr in
  Printf.printf "%s\n" (Vie_parser.Node.show_statement statement);