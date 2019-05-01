{
    open Parser
    exception Eof
}

rule token = parse
  | [' ' '\t']          { token lexbuf }
  | ['\n']              { EOL }
  | ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as s       { TIDENTIFIER (s)}
  | ['0'-'9']+ as lxm   { TINTEGER (int_of_string lxm) }
  | "="                 { TEQUAL }
  | "=="                 { TCEQ }
  | "!="                 { TCNE }
  | "<"                 { TCLT }
  | "<="                 { TCLE }
  | ">"                 { TCGT }
  | ">="                 { TCGE }
  | "+"                 { TPLUS }
  | "-"                 { TMINUS }
  | "*"                 { TMUL }
  | "/"                 { TDIV }
  | '('                 { TLPAREN }
  | ')'                 { TRPAREN }
  | '{'                 { TLBRACE }
  | '}'                 { TRBRACE }
  | '.'                 { TDOT }
  | ','                 { TCOMMA }
  | eof                 { EOF }
  | _                       { raise (Error (Printf.sprintf "Ree %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }
