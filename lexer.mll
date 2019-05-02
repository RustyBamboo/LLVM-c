{
    open Parser
    exception Eof
}

rule token = parse
  | [' ' '\t' '\n']          { token lexbuf }
  | ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as s       {  TIDENTIFIER (s)}
  | ['0'-'9']+ as lxm   { TINTEGER (int_of_string lxm) }
  | ['0'-'9']+'.'['0'-'9']* as lxm      { TDOUBLE(float_of_string lxm)}
  | "=" as s                    { TEQUAL(String.make 1 s) }
  | "==" as s                        { TCEQ(s) }
  | "!=" as s                        { TCNE(s) }
  | "<"  as s                        { TCLT(String.make 1 s) }
  | "<=" as s                        { TCLE(s) }
  | ">"  as s                        { TCGT(String.make 1 s) }
  | ">=" as s                        { TCGE(s) }
  | "+"  as s                        { TPLUS(String.make 1 s) }
  | "-"  as s                        { TMINUS(String.make 1 s) }
  | "*"  as s                        { TMUL(String.make 1 s) }
  | "/"  as s                        { TDIV(String.make 1 s) }
  | '('                         { TLPAREN }
  | ')'                         { TRPAREN }
  | '{'                         { TLBRACE }
  | '}'                         { TRBRACE }
  | ','                         { TCOMMA }
  | eof  {EOF}
