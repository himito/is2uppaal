%token <string> STRING
%token <int> INT
%token LESS GREATER SLASH EQ EOF
%token NAME START END
%token SCENARIO AUTOMATION
%token EVENT STARTEVENT ENDEVENT DATE
%token COMMAND ADDRESS
%token INTERVAL DMIN DMAX
%token CONDITION DISPOSE
%token CASE TRIGGER DEFAULT

%{
  open Lexer
  open Types
  let relations = ref []
  let hash_events = Hashtbl.create  100
  let counter_condition = ref 0

  let getInterval n s e min max =
    if min = max then
      Rigid (n, s, e, string2time min)
    else begin
      if max = "0u" then
        Flexible (n, s, e, string2time min)
      else
        Semi (n, s, e, string2time min, string2time max)
    end
  let string2bool s =
    if s = "1" then true
    else false

  let setCondition condition event trigger default id_c =
    let time_e = getTimeEvent (Hashtbl.find hash_events event) in
    Hashtbl.replace hash_events event (ConditionalEvent (event, time_e, Some (condition, trigger, string2bool default, id_c)))
%}

%start <Types.program>main
%%

main:
  LESS SCENARIO NAME EQ n=STRING GREATER s=body LESS SLASH SCENARIO GREATER EOF { (hash_events, !relations, Structure ("Main", "start", "end", s)) }
;

body:
  | { [] }
  | b= body e= event      { b }    (* control points *)
  | b= body a= automation { b } (* textures *)
  | b= body i= interval   { b }
  | b= body s= scenario   { s::b } (* structure *)
  (* | b= body s= scenarioT  { s::b }    (* it's the same that automation *) *)
  | b= body c= condition  { b }
;


event:
  | LESS STARTEVENT NAME EQ n=STRING DATE EQ d=STRING SLASH GREATER { Hashtbl.add hash_events n (TimeEvent (n, string2time d)) }
  | LESS ENDEVENT NAME EQ n=STRING DATE EQ d=STRING SLASH GREATER { Hashtbl.add hash_events n (TimeEvent (n, string2time d)) }
  | LESS EVENT NAME EQ n=STRING DATE EQ d=STRING SLASH GREATER { Hashtbl.add hash_events n (TimeEvent (n, string2time d)) }
  | LESS EVENT NAME EQ n=STRING DATE EQ d=STRING CONDITION EQ STRING SLASH GREATER { Hashtbl.add hash_events n (ConditionalEvent (n, string2time d, None) )}
  | LESS EVENT NAME EQ n=STRING DATE EQ d=STRING GREATER c=command LESS SLASH EVENT GREATER { Hashtbl.add hash_events n (CommandEvent (n, string2time d))}
;

command:
  LESS COMMAND ADDRESS EQ a=STRING GREATER v=INT LESS SLASH COMMAND GREATER  { }
;

automation:
  | LESS AUTOMATION NAME EQ n=STRING START EQ s=STRING END EQ e=STRING SLASH GREATER { }
  | LESS AUTOMATION NAME EQ n=STRING START EQ s=STRING END EQ e=STRING GREATER c= curve LESS SLASH AUTOMATION GREATER { }
;

curve:
  {}
;

interval:
  | LESS INTERVAL NAME EQ n=STRING START EQ s=STRING END EQ e=STRING DMIN EQ dmin=STRING DMAX EQ dmax=STRING SLASH GREATER { relations := (getInterval n s e dmin dmax)::!relations}
  | LESS INTERVAL NAME EQ n=STRING START EQ s=STRING END EQ e=STRING SLASH GREATER { relations := Flexible (n,s,e,0)::!relations}
;

(* Structure *)
scenario:
  | LESS SCENARIO NAME EQ n=STRING START EQ s=STRING END EQ e=STRING GREATER b=body LESS SLASH SCENARIO GREATER { Structure (n,s,e,b)} (* Structure*)
  | LESS SCENARIO NAME EQ n=STRING START EQ s=STRING END EQ e=STRING SLASH GREATER  { Texture (n, s, e)}
;

condition:
  LESS CONDITION NAME EQ n=STRING DISPOSE EQ STRING GREATER c=case LESS SLASH CONDITION GREATER { let (e,t,d) = c in setCondition n e t d !counter_condition; counter_condition := !counter_condition + 1}
;

case:
  LESS CASE EVENT EQ e=STRING TRIGGER EQ t=STRING DEFAULT EQ d=STRING SLASH GREATER { (e,t,d) }
;

