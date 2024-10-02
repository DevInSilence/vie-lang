%{
  (*
open Node
open Token
open Location*)
%}

%token <Token.t * Location.t> Number Decimal Iden
%token <Token.t * Location.t> Operator
%token Assign
%token EOF

%type <Node.statement list> program
%start program

%%

program:
  | EOF { [] }
