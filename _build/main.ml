open Lexing
open Parser
open Lexer
open Poly
open Expr

let filename = Sys.argv.(1)

let () = 
  open_in filename |>
  Lexing.from_channel |>
  Parser.main Lexer.token |>
  print_expr |>
  from_expr |>
  simplify |>
  print_pExp
