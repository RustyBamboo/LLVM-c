open Lexing
open Parser
open Lexer
open Codegen
open Expr

let filename = Sys.argv.(1)

let () = 
  open_in filename |>
  Lexing.from_channel |>
  Parser.program Lexer.token |>
  print_block |>
  codegen
