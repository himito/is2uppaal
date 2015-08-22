
type id_t = string
type id_i = int
type time_t = int

type case_t = Some of string * string * bool * id_i | None (* condition, trigger, default, id *)

type event_t = TimeEvent of id_t * time_t (* name, date *)
               | ConditionalEvent of id_t * time_t * case_t (* name, date, case, id *)
               | CommandEvent of id_t * time_t (* name, date *)

let getTimeEvent e =
  match e with
  | TimeEvent (n, d) -> d
  | ConditionalEvent (n, d, c) -> d
  | CommandEvent (n, d) -> d


type relation_t = Rigid of id_t * id_t * id_t * time_t (* name, start_event, end_event, duration *)
                  | Semi of id_t * id_t * id_t * time_t * time_t(* name, start_event, end_event, min, max *)
                  | Flexible of id_t * id_t * id_t * time_t (* name, start_event, end_event, min *)



type temporal_object_t = Texture of id_t * id_t * id_t (* name, start_event, stop_event *)
                         | Structure of id_t * id_t * id_t * scenario_t (* name, start_event, stop_event *)
and scenario_t = temporal_object_t list


type program = (id_t, event_t) Hashtbl.t * relation_t list  * temporal_object_t


let string2time s =
  let r = Str.regexp "[a-zA-Z]+" in
  let t = Str.global_replace r "" s in
  int_of_string t


let rec printScenario scenario p =
  let print_to t =
    match t with
    | Texture (n,s,e) -> print_string ((n)^" - parent: "^p^"\n")
    | Structure (n, s, e, l) -> print_string (n^"- parent: "^p^"\n")(*; printScenario l n*)
  in
  print_endline "print scenario started ...";
  List.iter print_to scenario;
  print_endline "print scenario finished ..."
