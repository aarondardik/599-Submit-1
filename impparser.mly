%token RPAREN
%token LPAREN
%token PLUS
%token MINUS

%token <string> VAR
%token <bool> BOOL

%token LT 
%token LEQ
%token EQ
%token AND
%token OR
%token NOT
%token OUTPUT
%token SKIP
%token SEQ
%token IF 
%token THEN
%token ELSE
%token FI
%token WHILE 
%token DO
%token DONE
%token ASGN


%token <int> INT
%token EOF

%start <Imp.cmd> prog

%%

prog:
  | e = cmd1; EOF { e }
  ;

(* Initially on the EQ command I had e1 = VAR...experimenting with change *)

cmd1:
  | e = cmd { e }
  | e1 = cmd1; SEQ; e2 = cmd { Imp.Seq(e1, e2) }
  ;

cmd:
  | OUTPUT; e = aexp { Imp.Output(e) }
  | SKIP; { Imp.Skip }
  | e1 = VAR; ASGN; e2 = aexp { Imp.Asgn(e1, e2) }
  | IF; e1 = bexp3; THEN; e2 = cmd1; ELSE; e3 = cmd; FI {Imp.IfElse(e1, e2, e3)}
  | WHILE; e1 = bexp3; DO; e2 = cmd; DONE { Imp.While(e1, e2) }
  | LPAREN; e = cmd1; RPAREN; { e }
  ;

aexp:
  | e = expr0 { e } 
  | el = aexp; PLUS; er = expr0 { Imp.Plus(el, er) }
  | el = aexp; MINUS; er = expr0 { Imp.Minus(el, er) }
  ;

expr0:
  | s = INT { Imp.Int s }
  | s = VAR { Imp.Var s }
  | LPAREN; e = aexp; RPAREN { e }
  ;

(* above we had s = INT { Imp.Int s } ...we are trying something else. change 
back if it doesnt work *)


bexp3:
  | e = bexp2 { e }
  | e1 = bexp3; OR; e2 = bexp2 { Imp.Or(e1, e2) }
  ;

bexp2:
  | e1 = bexp2; AND; e2 = bexp1 { Imp.And(e1, e2) }
  | e = bexp1 { e }
  ;

bexp1:
  | e = bexp0 { e }
  | NOT; e = bexp0 { Imp.Not(e) }
  ;

bexp0:
  | e = bexp { e }
  | e1 = aexp; LT; er = aexp {Imp.Lt (e1, er)}
  | e1 = aexp; LEQ; er = aexp {Imp.Leq (e1, er)}
  | e1 = aexp; EQ; er = aexp {Imp.Eq (e1, er)}  
  ;



bexp:
  | s = BOOL { Imp.Bool s }
  | LPAREN; e = bexp; RPAREN { e }
  ;





