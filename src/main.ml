open Parsing

let parse s =
  let lexbuf = Lexing.from_string s in
  Parser.prog Lexer.read lexbuf

let print_parse _ = failwith "TODO: unimplemented"

let options = [("-p", Arg.String print_parse, "Parse input and print AST")]

let unrecognised x = "unrecognised option " ^ x ^ ", use -help" |> print_endline

let main () = Arg.parse options unrecognised "Usage: riftc [options] FILE"

let _ = main ()
