(** Lexer for Mini-ML *)

{
  open Lexing
  open Parse
  open Common.Source

  let pos p =
    { file = p.pos_fname; line = p.pos_lnum; column = p.pos_cnum }
  let region lb =
    { left = pos (lexeme_start_p lb); right = pos (lexeme_end_p lb) }
}

let digit = ['0'-'9']
let char = ['a'-'z' '_' 'A'-'Z']

rule token = parse
| [' ' '\t' '\r']+
  { token lexbuf }
| '\n'
  { new_line lexbuf; token lexbuf }
| digit+ as s
  { CST (Bigint.of_string s) }
| "let"
  { LET }
| "in"
  { IN }
| "fun"
  { FUN }
| char+ as s
  { ID s }
| '+'
  { ADD }
| '('
  { LPAR }
| ')'
  { RPAR }
| ','
  { COMMA }
| '='
  { EQUAL }
| "->"
  { ARROW }
| _ as c
  { Error.error (region lexbuf) (Printf.sprintf "lexical error: %c" c) }
| eof
  { EOF }

{

  let parse_file ~handler filename =
    handler @@ fun () ->
    let c = open_in filename in
    let lb = from_channel c in
    set_filename lb filename;
    try let e = Parse.prog token lb in close_in c; Ok e with
    | Parsing.Parse_error ->
      Error (Spectec.Error.TaskParseError (region lb, "syntax error"))
    | Error.MinimlParseError (at, msg) ->
      Error (Spectec.Error.TaskParseError (at, msg))

  let parse_string ~spec:_ ~filename content =
    let lb = from_string content in
    set_filename lb filename;
    try Ok [Parse.prog token lb] with
    | Parsing.Parse_error ->
      Error (Spectec.Error.TaskParseError (region lb, "syntax error"))
    | Error.MinimlParseError (at, msg) ->
      Error (Spectec.Error.TaskParseError (at, msg))

}

