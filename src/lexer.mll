
{
open Lexing
open Parser
}

let start = ['a'-'z' 'A'-'Z' '_']
let end = start | ['0'-'9'] | '-'
let name = start end*

let newline = '\n' | '\r' | "\r\n"
let white = [' ' '\t']+

rule read = parse
    | newline   { new_line lexbuf; read lexbuf }
    | white     { read lexbuf }
    | "("       { LPAR }
    | ")"       { RPAR }
    | "\\"      { BACKSLASH }
    | ":"       { COLON }
    | ":="      { COLONEQUAL }
    | "."       { DOT }
    | "*"       { STAR }
    | "->"      { ARROW }
    | "!"       { BANG }
    | "eval"    { EVAL }
    | "load"    { LOAD }
    | "--"      { skip_line lexbuf }
    | name      { ID (Lexing.lexeme lexbuf) }
    | eof       { EOF }
    | _         { raise Parser.Error }

and skip_line = parse
    | newline   { new_line lexbuf; read lexbuf }
    | eof       { EOF }
    | _         { skip_line lexbuf }

