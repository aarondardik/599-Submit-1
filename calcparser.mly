%token RPAREN
%token LPAREN
%token PLUS
%token MINUS
%token MULT 
%token <int> INT
%token EOF

%start <Calc.expr> prog

%%

prog:
  | e = expr; EOF { e }
  ;

expr:
  | e = expr1 { e }
  ;

expr1:
  | e = expr0 { e }
  | el = expr6; PLUS; er = expr6 { Calc.Plus(el, er) }
  | el = expr6; MINUS; er = expr6 { Calc.Minus(el, er) }
  ;

expr6:
  | e = expr1 { e }
  | e1 = expr6; MULT; e2 = expr0 {Calc.Mult(e1, e2)}
  ;

expr0:
  | s = INT { Calc.Int s}
  | LPAREN; e = expr1; RPAREN { e }
  ;
