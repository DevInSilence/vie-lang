%{
%}

%token <Token.t> Number Decimal Iden
%token <Token.t> Operator
%token Assign
%token EOF

%type <Node.statement list> program
%start program

%%

program:
  | stmts=statement_list EOF { stmts }

statement_list:
  | stmt=statement { [stmt] }
  | stmt=statement stmts=statement_list { stmt :: stmts }

statement:
  | expr=expression { Node.Expression(expr) }

expression:
  | value_expr=value_expression { Node.ValueExpr(value_expr) }

value_expression:
  | intValue=Number { Node.IntegerExpr(intValue) }
  | floatValue=Decimal { Node.FloatExpr(floatValue) }
