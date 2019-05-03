open Lexing
open Parser
open Lexer
open Codegen
open Expr

let filename = Sys.argv.(1)
let filename_llvm = Sys.argv.(2)

let () = 
    let block = open_in filename |>
  Lexing.from_channel |>
  Parser.program Lexer.token in
  (*|> print_block in*)
  codegen_main block filename_llvm
