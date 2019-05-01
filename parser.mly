%{
    open Expr
%}

%token <int> TINTEGER
%token <float> TDOUBLE
%token <string> TIDENTIFIER
%token <string> TPLUS TMINUS TMUL TDIV
%token TLPAREN TRPAREN TLBRACE TRBRACE
%token <string> TEQUAL TCEQ TCNE TCLT TCLE TCGT TCGE
%token TCOMMA EOF

%left TPLUS TMINUS
%left TMUL TDIV

%type <string> ident
%type <Expr.expr> numeric expr 
%type <Expr.statement list> func_decl_args
%type <Expr.expr list> call_args
%type <Expr.block> program stmts block
%type <Expr.statement> stmt var_decl func_decl
%type <string> comparison

%start program

%%

program: stmts EOF                   { $1 }
    ;

stmts : stmt { [$1] }
    | stmts stmt { $1@[$2] }
    ;

stmt : var_decl {$1} | func_decl {$1}
    | expr { Expr($1) }
    ;

block : TLBRACE stmts TRBRACE { $2 }
    | TLBRACE TRBRACE { [] }
    ;

var_decl : ident ident { VariableDeclaration($1, $2) }
    | ident ident TEQUAL expr { VariableDeclarationExpr($1, $2, $4) }
    ;

func_decl : ident ident TLPAREN func_decl_args TRPAREN block 
                {FunctionDeclaration($1, $2, $4, $6)}
    ;

func_decl_args : {[]}
    | var_decl {[$1]}
    | func_decl_args TCOMMA var_decl {$1@[$3]}
    ;

ident : TIDENTIFIER { $1 }
    ;

numeric : TINTEGER { Int($1) }
    | TDOUBLE {Double($1)}
    ;

expr : ident TEQUAL expr {Assignment($1, $3)}
    | ident TLPAREN call_args TRPAREN {MethodCall($1, $3)}
    | ident {Identifier($1)}
    | numeric {$1}
    | expr comparison expr {BinaryOperator($1, $2, $3)}
    | TLPAREN expr TRPAREN {$2} 
    ;

call_args : {[]}
    | expr {[$1]}
    | call_args TCOMMA expr {$1@[$3]}
    ;

    comparison : TCEQ {$1} | TCNE {$1} | TCLT {$1} | TCLE {$1} | TCGT {$1} | TCGE {$1} | TPLUS {$1} | TMINUS {$1} | TMUL {$1} | TDIV {$1}
           ;

%%
