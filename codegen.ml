open Llvm


exception Error of string

let context = global_context ()
let the_module = create_module context "my cool jit"
let builder = builder context
let named_values:(string, llvalue) Hashtbl.t = Hashtbl.create 10
let double_type = double_type context

let codegen_expr (e: Expr) =
    match e with
    | Int n -> const_float int_type n
    | Double n -> const_float double_type n
    | MethodCall (callee, args) -> 
            let callee =  match lookup_function callee the_module with
                            | Some callee -> callee
                            | None -> raise (Error "unknown function referenced")
            in
            let args = Array.map codegen_expr args in
            build_call callee args "calltmp" builder
    | BinaryOperator (lhs, op, rhs) ->
            let lhs_val = codegen_expr lhs in
            let rhs_val = codegen_expr rhs in
            (
                match op with
                    | "+" -> build_add lhs_val rhs_val "addtmp" builder
                    | "-" -> build_sub lhs_val rhs_val "subtmp" builder
                    | "*" -> build_mul lhs_val rhs_val "multmp" builder
            )
    | Identifier name ->
            let v = try Hashtbl.find named_values name with
                | Not_found -> raise (Error "unknown variable name")
            in
            (* Load the value. *)
            build_load v name builder
    | _ -> failwith "codegen expr failed"

let codegen_statement (s: statement) =
    match s with
    | Expr e -> codegen_expr e
    | VariableDeclarationExpr (t, n, e) -> 
            let expr_val = codegen_expr e in
            Hashtbl.add named_values n e;
            e
    | VariableDeclaration (t, n) -> 
            let expr_val = codegen_expr Int(0) in
            Hashtbl.add named_values n e;
            e
    | FunctionDeclaration (t, n, args, code) -> codegen_expr code
    _ -> failwith "oof"



let codegen e = Printf.printf "reeeeee"
