
type expr = 
  | Int of int
  | Double of float
  | Assignment of string * expr
  | MethodCall of string * expr list
  | BinaryOperator of expr * string * expr
  | Identifier of string

type variableList = string * string * expr list 

type statement = 
    | Expr of expr
    | VariableDeclarationExpr of string * string * expr (* type, id, assignment expr *)
    | VariableDeclaration of string * string
    | FunctionDeclaration of string * string * statement list * statement list

type block = statement list


let rec 
  print_expr_r (el : expr list) : unit =
      match el with
      | hd::tl -> print_expr hd; print_expr_r tl
      | [] -> ()
and 
  print_expr (e:expr) : unit =
      match e with
      | Int(i) -> Printf.printf " %i " i
      | Double(d) -> Printf.printf " %f " d
      | Assignment(s, e) -> Printf.printf " assign %s" s; print_expr(e); print_newline();
      | MethodCall (s, el) -> Printf.printf " methodcall %s" s; print_expr_r el; print_newline ();
      | BinaryOperator (e, s, e2) -> print_expr e; Printf.printf  " %s " s; print_expr e2;
      | Identifier(s) -> Printf.printf " id(%s) " s

and 

  print_statement (s: statement): unit =
      match s with 
      | Expr(expr) -> Printf.printf "Expr"; print_expr expr
      | VariableDeclarationExpr(a, b, e) -> Printf.printf "VarDecExpr: %s %s =" a b; print_expr e; print_newline ();
      | VariableDeclaration(a, b) -> Printf.printf "VarDec: %s %s" a b; print_newline ();
      | FunctionDeclaration(a, b, args, code) -> Printf.printf "FuncDec: %s %s\n" a b; Printf.printf "\targs:"; print_block_r args; Printf.printf "\n\tcode: "; print_block_r code; Printf.printf "\n";
  and 
  print_block_r (b:block): unit = 
    match b with
    | hd::tl -> (print_statement hd); print_block_r tl
    | [] -> ()

(*
  Print expression and pass it through
*)
let print_block (b:block) = 
  print_block_r b;
  print_newline ();
  b
