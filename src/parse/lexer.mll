{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
              pos_lnum = pos.pos_lnum + 1
    }
}

let digit = ['0'-'9']
let letter = ['A'-'Z' 'a'-'z']

let int = '-'? ('0' | digit+)
let id = (letter | '_') (letter | digit | '_')*
let type_id = ['A'-'Z'] (letter | digit | '_')*

let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read = parse
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "[" { LBRACKET }
  | "]" { RBRACKET }
  | "{" { LCURLY }
  | "}" { RCURLY }
  | "," { COMMA }
  | ":" { COLON }
  | ";" { SEMICOLON }
  | "." { DOT }
  | "=>" { ARROW }
  | "|" { BAR }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { MULT }
  | "/" { DIV }
  | "%" { REM }
  | "<" { LT }
  | "<=" { LE }
  | ">" { GT }
  | ">=" { GE }
  | "&&" { AND }
  | "||" { OR }
  | "==" { EQ }
  | "!=" { NOTEQ }
  | "!" { NOT }
  | "=" { ASSIGN }
  | "i64" { TYPE_INT }
  | "bool" { TYPE_BOOL }
  | "string" { TYPE_STRING }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "while" { WHILE }
  | "do" { DO }
  | "let" { LET }
  | "in" { IN }
  | "end" { END }
  | "break" { BREAK }
  | "fn" { FN }
  | "extern" { EXTERN }
  | "var" { VAR }
  | "val" { VAL }
  | "match" { MATCH }
  | "with" { WITH }
  | "type" { TYPE }
  | "true" { TRUE }
  | "false" { FALSE }
  | whitespace { read lexbuf }
  | "#" { read_single_line_comment lexbuf }
  | int { INT_LIT(Int64.of_string (Lexing.lexeme lexbuf)) }
  | type_id { TYPE_ID (Lexing.lexeme lexbuf) }
  | id { ID (Lexing.lexeme lexbuf) }
  | '"' { read_string_lit (Buffer.create 17) lexbuf }
  | newline { next_line lexbuf; read lexbuf }
  | eof { EOF }
  | _ { raise (SyntaxError ("Illegal character: " ^ Lexing.lexeme lexbuf)) }

and read_single_line_comment = parse
  | newline { next_line lexbuf; read lexbuf }
  | eof { EOF }
  | _ { read_single_line_comment lexbuf }

and read_string_lit buf = parse
  | '"' { STRING_LIT (Buffer.contents buf) }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string_lit buf lexbuf }
  | '\\' '"' { Buffer.add_char buf '"'; read_string_lit buf lexbuf }
  | '\\' 'n' { Buffer.add_char buf '\n'; read_string_lit buf lexbuf }
  | '\\' 't' { Buffer.add_char buf '\t'; read_string_lit buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string_lit buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String literal is not terminated.")) }
