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
%token REF ASSIGN DEREF

%token TUNIT
%token TINT
%token TBOOL

%left ELSE IN ARROW
%left SEMICOLON
%left ASSIGN
%nonassoc NOT
%nonassoc EQ NEQ LESS LESSEQ
%left OR
%left AND
%left PLUS MINUS
%left MULT DIV
%nonassoc REF
%nonassoc DEREF


%start prog
%type <Syntax.exprML> prog


%%

prog: expr; EOF  { $1 }

expr:
  | app_expr { $1 }
  | expr SEMICOLON expr         { Seq ($1, $3) }
  | LPAR expr COMMA expr RPAR   { Pair ($2, $4) }
  | IF expr THEN expr ELSE expr        { If ($2, $4, $6) }
  | FUN LPAR VAR COLON ty RPAR ARROW expr { Fun ($3, $5, $8) }
  | FIX VAR LPAR VAR COLON ty RPAR COLON ty ARROW expr { Fix ($2,$9, $4, $6, $11) }
  | LET VAR EQ expr IN expr { Let ($2, $4, $6) }
  | REF expr         { Newref $2 }    
  | expr ASSIGN expr { Assign ($1,$3) }
  | DEREF expr       { Deref $2 }  
(*  | MINUS expr          { UMinus (-$2) }  *)
  | expr PLUS expr     { Plus ($1, $3) }
  | expr MINUS expr    { Minus ($1, $3) }
  | expr MULT expr     { Mult ($1, $3) }
  | expr DIV expr      { Div ($1, $3) }
  | NOT expr          { Not ($2) }
  | expr AND expr     { And ($1, $3) }
  | expr OR expr      { Or ($1, $3) }   
  | expr EQ expr      { Equal ($1, $3) }
  | expr NEQ expr     { NEqual ($1, $3) }  
  | expr LESS expr    { Less ($1, $3) }
  | expr LESSEQ expr  { LessEq ($1, $3) }   

app_expr:
  | simple_expr { $1 }
  | app_expr simple_expr         { App ($1, $2) }

simple_expr:
  | VAR             { Var $1 }
  | UNIT            { Unit }
  | INT             { Int $1 }
  | TRUE            { Bool true }
  | FALSE           { Bool false }    
  | LPAR expr RPAR   { $2 }


ty:
  | TUNIT        { TUnit }
  | TBOOL        { TBool }
  | TINT         { TInt }
  | REF ty      { TRef $2 }
  | ty ARROW ty { TArrow ($1, $3) }
  | ty MULT ty   { TProd ($1, $3) }  
  | LPAR ty RPAR { $2 }
  
%%  
