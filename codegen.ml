open Llvm
open Llvm_bitwriter
open Expr


exception Error of string

type tvals = string * llvalue

let context = global_context ()
let the_module = create_module context "awesome jit"
let builder = builder context
let named_values:(string, tvals) Hashtbl.t = Hashtbl.create 10
let double_type = double_type context
let int_type = i32_type context
let void_type = void_type context

let get_type t =
    match t with 
    | "int" -> int_type
    | "double" -> double_type
    | _ -> failwith "unsupported type"
let rec create_list_of_types args =
    match args with
    | [] -> []
    | hd::tl -> 
            ( match hd with 
            | VariableDeclaration (t, n) -> [(get_type t)]@(create_list_of_types tl)
            | _ -> failwith "not type in args?"
            )
let rec create_list_of_arg_names args=
    match args with
    | [] -> []
    | hd::tl -> 
            ( match hd with 
            | VariableDeclaration (t, n) -> [(t, n)]@(create_list_of_arg_names tl)
            | _ -> failwith "not type in args?"
            )


let rec codegen_expr (e: expr) =
    match e with
    | Int n -> const_int int_type n
    | Double n -> const_float double_type n
    | MethodCall (callname, args) -> 
            (*Printf.eprintf "Calling function %s\n" callname;*)
            let callee =  match lookup_function callname the_module with
                            | Some callee -> callee
                            | None -> raise (Error "unknown function referenced")
            in
            let params = params callee in
            if Array.length params == List.length args then () else
                raise (Error "incorrect # args");
            let args = List.map codegen_expr args in
            if callname = "printf" then
                let printval = List.hd args in
                match classify_type (type_of printval) with
                | TypeKind.Integer ->
                    let const_int n = const_int int_type n in
                    let str s = define_global "buf" (const_stringz context s) the_module in
                    let int_spec = build_gep (str "%d\n") [| const_int 0; const_int 0 |] "int_spec" builder in
                build_call callee [| int_spec;  (List.hd args) |] "" builder
                | _ ->
                    let const_int n = const_int int_type n in
                    let str s = define_global "buf" (const_stringz context s) the_module in
                    let int_spec = build_gep (str "%f\n") [| const_int 0; const_int 0 |] "float_spec" builder in
                build_call callee [| int_spec;  (List.hd args) |] "" builder

            else
            build_call callee (Array.of_list args) "calltmp" builder
    | BinaryOperator (lhs, op, rhs) ->
            (*Printf.eprintf "Operator %s\n" op;*)
            let lhs_val = codegen_expr lhs in
            let rhs_val = codegen_expr rhs in
            (*Printf.eprintf " type LHS %s" (string_of_lltype (type_of lhs_val));
            Printf.eprintf " type RHS %s\n" (string_of_lltype (type_of rhs_val));*)
            let w =
                match classify_type (type_of lhs_val), classify_type (type_of rhs_val) with
                | TypeKind.Integer, TypeKind.Integer -> 0
                | _, TypeKind.Double -> 1
                | TypeKind.Double, _ -> 1
                | _,_ -> 1
            in
            (
                match op with
                    | "+" -> if w = 0 then build_add lhs_val rhs_val "addtmp" builder else build_fadd lhs_val rhs_val "addtmp" builder
                    | "-" -> if w = 0 then build_sub lhs_val rhs_val "subtmp" builder else build_fsub lhs_val rhs_val "subtmp" builder
                    | "*" -> if w = 0 then build_mul lhs_val rhs_val "multmp" builder else build_fmul lhs_val rhs_val "multmp" builder
                    | _ -> failwith "oof"
            )
    | Identifier name ->
            (*Printf.eprintf "Variable %s\n" name;*)
            let v = try Hashtbl.find named_values name with
                | Not_found -> raise (Error "unknown variable name")
            in
            match v with
            | t, v -> (*build_load v name builder*) v
            | _ -> failwith "codegen expr failed"
    | _ -> failwith "codegen expr failed"

and codegen_statement (s: statement) =
    match s with
    | Expr e -> codegen_expr e
    | VariableDeclarationExpr (t, n, e) -> 
            (*Printf.eprintf "Setting variable %s\n" n;*)
            let expr_val = codegen_expr e in
            Hashtbl.add named_values n (t, expr_val);
            expr_val
    | VariableDeclaration (t, n) -> 
            (*Printf.eprintf "Variable declaration %s\n" n;*)
            let expr_val = codegen_expr (Int(0)) in
            Hashtbl.add named_values n (t, expr_val);
            expr_val
    | FunctionDeclaration (t, name, args, code) -> 
            (*Printf.eprintf "Declaring function %s: %s\n" t name;*)
            let arg = create_list_of_types args in
            let ft = function_type (get_type t) (Array.of_list arg) in
            let f =
        match lookup_function name the_module with
            | None -> declare_function name ft the_module
            | Some f ->
                if block_begin f <> At_end f then
                    raise (Error "redefinition of function");
                if element_type (type_of f) <> ft then
                    raise (Error "redefinition of function with different # args");
                f
            in
                 let args = Array.of_list (create_list_of_arg_names args) in 
                 (* Set names for all arguments. *)
                 Array.iteri (fun i a ->
                   let t,n = args.(i) in
                   set_value_name n a;
                   Hashtbl.add named_values n (t, a);
                 ) (params f);
            let the_function = f in 
                 let bb = append_block context "entry" the_function in
                 position_at_end bb builder;
                 let noinline = create_enum_attr context "noinline" 0L in
                 let nounwind = create_enum_attr context "nounwind" 0L in
                 let optnone = create_enum_attr context "optnone" 0L in
                 let sspstrong = create_enum_attr context "sspstrong" 0L in
                 let uwtable = create_enum_attr context "uwtable" 0L in
                 add_function_attr the_function noinline AttrIndex.Function;
                 add_function_attr the_function nounwind AttrIndex.Function;
                 add_function_attr the_function optnone AttrIndex.Function;
                 add_function_attr the_function sspstrong AttrIndex.Function;
                 add_function_attr the_function uwtable AttrIndex.Function;

                 try
                   let ful = List.map codegen_statement code in

                   (* return type of last expression*)
                   let ret_val = (List.hd (List.rev ful)) in

                   (* Finish off the function. *)
                   let _ = build_ret ret_val builder in

                   (* Validate the generated code, checking for consistency. *)
                   Llvm_analysis.assert_valid_function the_function;

                   the_function
                 with e ->
                   delete_function the_function;
                   raise e




let rec codegen_main_r (b:block) = 
    match b with
    | [] ->()
    | hd::tl -> (codegen_statement hd); codegen_main_r tl



let codegen_main (b: block) out_llvm_filename =

let print =   let string = pointer_type (i8_type context) in declare_function "printf" (var_arg_function_type int_type [|string|]) the_module in
    let _ = codegen_main_r b in
    let _ = write_bitcode_file the_module out_llvm_filename in ()


