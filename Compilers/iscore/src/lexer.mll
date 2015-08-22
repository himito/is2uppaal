{
  type token =
    | STRING of string
    | INT of int
    | LESS
    | GREATER
    | SLASH
    | EQ
    | NAME
    | START
    | END
    | SCENARIO
    | AUTOMATION
    | EVENT
    | STARTEVENT
    | ENDEVENT
    | DATE
    | COMMAND
    | ADDRESS
    | INTERVAL
    | DMIN
    | DMAX
    | CONDITION
    | DISPOSE
    | CASE
    | TRIGGER
    | DEFAULT
    | EOF
}


rule lex = parse
  | [' ' '\t' '\n']+         { lex lexbuf }
  | '"'                      { read_string (Buffer.create 20) lexbuf }
  | ['0'-'9']['0'-'9']* as i { INT (int_of_string (i)) }
  | "<"                      { LESS }
  | ">"                      { GREATER }
  | "/"                      { SLASH}
  | "="                      { EQ }
  | "name"                   { NAME }
  | "start"                  { START }
  | "end"                    { END }
  (* scenario *)
  | "Scenario"               { SCENARIO }
  (* automation *)
  | "Automation"             { AUTOMATION }
  (* event *)
  | "event"                  { EVENT }
  | "startEvent"             { STARTEVENT }
  | "endEvent"               { ENDEVENT }
  | "date"                   { DATE }
  (* command *)
  | "command"                { COMMAND }
  | "address"                { ADDRESS }
  (* interval *)
  | "Interval"               { INTERVAL }
  | "durationMin"            { DMIN }
  | "durationMax"            { DMAX }
  (* condition *)
  | "condition"              { CONDITION }
  | "dispose"                { DISPOSE }
  (* case *)
  | "case"                   { CASE }
  | "trigger"                { TRIGGER }
  | "default"                { DEFAULT }
  | eof                      { EOF }



and read_string buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }

