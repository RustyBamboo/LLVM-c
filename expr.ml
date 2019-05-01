
type ident = string

type expr = 
  | Int of int
  | Double of float
  | Assignment of ident * expr
  | MethodCall of ident * expr list
  | BinaryOperator of string * expr * expr
  | Identifier of ident

type variableList = ident * ident * expr list 

type statement = 
    | Expr of expr
    | VariableDeclarationExpr of ident * ident * expr (* type, id, assignment expr *)
    | VariableDeclaration of ident * ident
    | FunctionDeclaration of ident * ident * variableList * statement list

type block = statement list

