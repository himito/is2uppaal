(* This exception is raised by the monolithic API functions. *)
exception Error

(* The monolithic API. *)
val main: (Lexing.lexbuf -> Lexer.token) -> Lexing.lexbuf -> (Types.program)

