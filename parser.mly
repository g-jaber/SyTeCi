%{
  open Syntax
%} 


%token EOF
%token <int> INT
%token <Syntax.id> VAR
%token EQ
%token PLUS MINUS MULT DIV
%token AND OR NOT
%token NEQ LESS LESSEQ
%token TRUE FALSE
%token LPAR RPAR COMMA COLON SEMICOLON
%token LET IN
%token FUN FIX ARROW
%token IF THEN ELSE
%token UNIT
%token REF
%token ASSIGN
%token DEREF

%token TUNIT
%token TINT
%token TBOOL



%start prog
%type <Syntax.exprML> prog

%nonassoc NOT
%nonassoc ARROW
%nonassoc IN
%nonassoc ELSE
%nonassoc EQ NEQ LESS LESSEQ
%left OR
%left AND
%left PLUS MINUS
%left MULT DIV
%nonassoc REF

%%

prog: expr; EOF  { $1 }

expr:
  | expr0               { $1 }
  | arith               { $1 }
  | boolean             { $1 }
  | LPAR expr COMMA expr RPAR   { Pair ($2, $4) }
  | IF expr THEN expr ELSE expr        { If ($2, $4, $6) }
  | FUN LPAR VAR COLON ty RPAR ARROW expr { Fun ($3, $5, $8) }
  | FIX VAR LPAR VAR COLON ty RPAR COLON ty ARROW expr { Fix ($2,$9, $4, $6, $11) }
  | LET VAR EQ expr IN expr { Let ($2, $4, $6) }

expr0:  
  | expr1 SEMICOLON expr0         { Seq ($1, $3) }
  | expr1        { $1 }

expr1:
  | expr1 expr2         { App ($1, $2) }
  | expr2        { $1 }  

expr2:
  | expr3 ASSIGN expr3 { Assign ($1,$3) }
  | expr3        { $1 }    


expr3:
  | VAR             { Var $1 }
  | UNIT            { Unit }
  | TRUE            { Bool true }
  | FALSE           { Bool false }
  | INT             { Int $1 }
  | REF expr3         { Newref $2 }    
  | DEREF expr3       { Deref $2 }  
  | LPAR expr RPAR   { $2 }      
  
arith:
  | MINUS INT          { Int (-$2) }
  | expr PLUS expr     { Plus ($1, $3) }
  | expr MINUS expr    { Minus ($1, $3) }
  | expr MULT expr     { Mult ($1, $3) }
  | expr DIV expr      { Div ($1, $3) }
  
boolean:
  | NOT expr          { Not ($2) }
  | expr AND expr     { And ($1, $3) }
  | expr OR expr      { Or ($1, $3) }   
  | expr EQ expr      { Equal ($1, $3) }
  | expr NEQ expr     { NEqual ($1, $3) }  
  | expr LESS expr    { Less ($1, $3) }
  | expr LESSEQ expr  { LessEq ($1, $3) }   

ty:
  | TUNIT        { TUnit }
  | TBOOL        { TBool }
  | TINT         { TInt }
  | REF ty      { TRef $2 }
  | ty ARROW ty { TArrow ($1, $3) }
  | ty MULT ty   { TProd ($1, $3) }  
  | LPAR ty RPAR { $2 }
  
%%  