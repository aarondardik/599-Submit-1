{

  open Lexing
  open Impparser

  exception SyntaxError of string

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with pos_bol = lexbuf.lex_curr_pos;
                                    pos_lnum = pos.pos_lnum + 1 }
}

let intPattern = ['0'-'9']+
let white = [' ' '\t']+
let letters = ['a'-'z' 'A'-'Z']
let unserscore = '_'
let var = letters+
let newline = '\n'

(*Other "var" below was var open brack read lexbuf close bracket *)

rule read =
  parse
  | white { read lexbuf }
  | newline { next_line lexbuf; read lexbuf }
  | intPattern { INT(int_of_string(Lexing.lexeme lexbuf)) }
  | var  {VAR(Lexing.lexeme lexbuf) }
  | '+' { PLUS }
  | '-' { MINUS }
  | '<' { LT }
  | '=' { ASGN }
  | "==" { EQ }
  | ';' { SEQ }
  | "<=" { LEQ }
  | "&&" { AND }
  | "||" { OR }
  | '!' { NOT }
  | "output" { OUTPUT }
  | "while" { WHILE }
  | "do" { DO }
  | "done" { DONE }
  | "Skip" { SKIP }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "fi" { FI }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | eof { EOF }
  | _ { raise (SyntaxError("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
