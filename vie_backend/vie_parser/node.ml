open Vie_lexer

type value_expr = 
  | IntegerExpr of Token.t
  | FloatExpr of Token.t
  [@@deriving show]

type variable_expr = Token.t [@@deriving show]

type operator = 
  | PlusOp
  | MinusOp
  | MultiplyOp
  | DivideOp
  [@@deriving show]


type binary_expr = {
  left : value_expr;
  operator : operator;
  right : value_expr;
} [@@deriving show]

type type_expr = 
  | IntType of Token.t
  | FloatType of Token.t
  [@@deriving show]

type expr = 
  | ValueExpr of value_expr
  | VariableExpr of variable_expr
  | BinaryExpr of binary_expr
  [@@deriving show]


type statement =
  | VariableDeclaration of (variable_expr * type_expr)
  | VariableAssignment of (variable_expr * type_expr option * expr)
  | Expression of expr
  [@@deriving show]
