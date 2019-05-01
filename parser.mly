%{
    open Expr
%}

%token <int> TINTEGER
%token <float> TDOUBLE
%token <string> TIDENTIFIER
%token TPLUS TMINUS TMUL TDIV
%token TLPAREN TRPAREN TLBRACE TRBRACE
%token TEQUAL TCEQ TCNE TCLT TCLE TCGT TCGE
%token TCOMMA
%left TPLUS TMINUS
%left TMUL TDIV

%type <Expr.ident> ident
%type <Expr.expr> numeric expr 
%type <Expr.variableList> func_decl_args
%type <Expr.expr list> call_args
%type <Expr.block> program stmts block
%type <Expr.statement> stmt var_decl func_decl
%type <token> comparison

%start program

%%

program: stmts                    { $1 }
    ;

stmts : stmt { [$1] }
    | stmts stmt { $1::[$2] }
    ;

stmt : var_decl | func_decl
    | expr { Expr($1) }
    ;

block : TLBRACE stmts TRBRACE { $2 }
    | TLBRACE TRBRACE { Block([]) }
    ;

var_decl : ident ident { VariableDeclaration($1, $2) }
    | ident ident TEQUAL expr { VariableDeclarationExpr($1, $2, $3) }
    ;

func_decl : ident ident TLPAREN func_decl_args TRPAREN block 
                {FunctionDeclaration($1, $2, $4, $6)}
    ;

func_decl_args : {[]}
    | var_decl {[$1]}
    | func_decl_args TCOMMA var_decl {$1::[$3]}
    ;

ident : TIDENTIFIER { Identifier($1) }
    ;

numeric : TINTEGER { Int($1) }
    | TDOUBLE {}
    ;

expr : ident TEQUAL expr {Assignment($1, $3)}
    | ident TLPAREN call_args TRPAREN {MethodCall($1, $3)}
    | ident {Identifier($1)}
    | numeric {}
    | expr comparison expr {BinaryOperator($1, $2, $3)}
    | TLPAREN expr TRPAREN {$2} 
    ;

call_args : {[]}
    | expr {[Expr($1)]}
    | call_args TCOMMA expr {$1::[$2]}
    ;

comparison : TCEQ | TCNE | TCLT | TCLE | TCGT | TCGE | TPLUS | TMINUS | TMUL | TDIV {$1}
           ;

%%
