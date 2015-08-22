open Types
open Lexer

(** function that parses the scenario **)
let process_of_file =
  let filename = Sys.argv.(1) in
  Parser.main Lexer.lex (Lexing.from_channel (open_in filename))


let (events, relations, scenario) = process_of_file

let getEvent i =
  Hashtbl.find events i

let isTimeEvent e =
  match getEvent e with
  | TimeEvent _ -> true
  | _ -> false

let isRigidInterval i =
  match i with
  | Rigid _ -> true
  | _ -> false


let getCondition e =
  let event = getEvent e in
  match event with
  | ConditionalEvent (n,t,c) -> c
  | _ -> None

let getIdCondition e =
  match getCondition e with
  | Some (cond, trig, defa, i) -> (cond,i)
  | None -> ("",0)

let isDefault e =
  match getCondition e with
  | Some (cond, trig, defa, i) -> defa
  | None -> false

let isCommandEvent e =
  match getEvent e with
  | CommandEvent _ -> true
  | _ -> false

let getDuration s e =
  let start_date = getTimeEvent (getEvent s) in
  let end_date = getTimeEvent (getEvent e) in
  end_date - start_date

(* Template for rigid intervals/ textures *)
let templateRigid name duration launch stop skip_p kill_p skip kill =
  Printf.printf "%s = Rigid(%d, %s, %s, %s, %s, %s, %s);\n" name duration launch stop skip_p kill_p skip kill

(* Template for flexible intervals/ textures/ structures*)
let templateFlexible name dmin dmax isFinite launch min timeout stop skip_p kill_p skip kill =
  Printf.printf "%s = Flexible(%d, %d, %b, %s, %s, %s, %s, %s, %s, %s, %s);\n" name dmin dmax isFinite launch min timeout stop skip_p kill_p skip kill

(* Template for controlling several intervals *)
let templateControl name n_relations start_c skip_p skip launch kill=
  (* let name = Random.bits() in *)
  Printf.printf "c%s = Control(%d, %s, %s, %s, %s, %s);\n" name n_relations start_c skip_p skip launch kill


(* Template for interaction points *)
let templateCondition name id_c en_e msg_e external_e default launch skip_p stop kill_p skip start =
  (* let name = Random.bits() in *)
  Printf.printf "cond%s = Point(%d, %s, %s, %s, %b, %s, %s, %s, %s, %s, %s);\n" name id_c en_e msg_e external_e default launch skip_p stop kill_p skip start

(* Template for mixing signals *)
let templateMix s_input skip kill s_output =
  let name = Random.bits() in
  Printf.printf "m%d = Mix(%s, %s, %s, %s);\n" name s_input s_output skip kill

(* Template for creating channels *)
let templateChannel name =
  Printf.printf "broadcast chan %s;\n" name


let templateVarInt name =
  Printf.printf "int %s;\n" name

let templateVarBool name =
  Printf.printf "bool %s;\n" name

let templateEvent event msg date value =
  let name = "e"^(string_of_int (Random.bits())) in
  Printf.printf "%s = Event(%s,%s,%d,%d);\n" name msg event date value



let createChannels events =
  Hashtbl.iter (fun a b -> templateChannel a) events

(* Return the start and end events of an interval *)
let getEventsInterval interval =
  match interval with
    | Rigid (n,s,e,d) -> (s,e)
    | Semi (n,s,e,min,max) -> (s,e)
    | Flexible (n,s, e, min) -> (s,e)

let getEventsTO t =
  match t with
  | Texture (n, s, e) -> (s,e)
  | Structure (n, s, e, l) -> (s,e)


(* Create the processes for a list of intervals *)
let rec createIntervals intervals kill_p =
  let allRigid l =
    List.for_all isRigidInterval l
  in
  let createInterval i =
    match i with
    | Rigid (n,s,e,d) ->
        let kill = "k_"^n in
        let skip = "skip_"^e in (* event sent by the point to the next temporal object *)
        templateChannel kill;
        templateChannel skip;
        templateRigid n d s e ("skip_"^s) kill_p skip kill
    | Semi (n,s,e,min,max) -> print_endline "TODO"
    | Flexible (n,s,e, min) ->
        let kill = "k_"^n in
        let skip = "skip_"^e in (* event sent by the point to the next temporal object *)
        let name_cond = string_of_int (Random.bits()) in
        let skip_c = "skip_"^name_cond in (* event sent by the interval to the point *)
        let time_out = "t"^name_cond in
        let start_c = "scond"^name_cond in
        let (id_cond,id_cond_i) = (getIdCondition e) in
        let en_c = "en_"^id_cond in
        templateChannel time_out;
        templateChannel kill;
        templateChannel skip;
        templateChannel skip_c;
        templateChannel start_c;
        templateVarBool en_c;
        templateFlexible n 0 min false s start_c e time_out ("skip_"^s) kill_p skip_c kill;
        templateCondition name_cond id_cond_i en_c ("msg_"^id_cond) id_cond (isDefault e) start_c skip_c time_out kill_p skip e
  in
  let createMergeIntervals l =
    (* controller *)
    let (se,ee) = getEventsInterval (List.hd l) in
    let name_c = string_of_int (Random.bits())  in
    let start_c = "sc"^name_c in
    let skip_c = "skip_"^name_c in (* event sent by the interval to the point *)
    let skip = "skip_"^ee in (* event sent by the point to the next temporal object *)
    templateChannel start_c;
    templateChannel skip_c;
    templateChannel skip;
    (* If all intervals are rigid *)
    if (allRigid l ) then
      begin
        templateControl name_c ((List.length l)) start_c skip_c skip ee kill_p;
        Printf.printf "// there are %d rigid intervals\n" (List.length l);
        List.iter (
          fun x ->
            match x with
              | Rigid (n,s,e,d) ->
                  let kill = "k_"^n in
                  templateChannel kill;
                  templateRigid  n d s start_c ("skip_"^s) kill_p skip_c kill
              | _ -> ()
        ) l
      end
    else
      begin
        Printf.printf "// there are %d flexible intervals\n" (List.length l);
        let name_cond = string_of_int (Random.bits()) in
        let time_out = "t"^name_cond in
        let skip_cond = "skip_"^name_cond in
        let (id_cond_ee, id_cond_ee_i) = getIdCondition ee in
        let start_cond = "s"^id_cond_ee in
        let en_c = "en_"^id_cond_ee in
        templateChannel time_out;
        templateChannel skip_cond;
        templateChannel start_cond;
        templateVarBool en_c;
        templateControl name_c ((List.length l)) start_c skip_c skip_cond start_cond kill_p;
        templateCondition name_cond id_cond_ee_i en_c ("msg_"^id_cond_ee) id_cond_ee (isDefault ee) start_cond skip_cond time_out kill_p skip ee;
        List.iter (
          fun x ->
            match x with
            | Semi (n,s,e,min,max) ->
              let kill = "k_"^n in
              templateChannel kill;
              templateFlexible n min max true s start_c ee time_out ("skip_"^s) kill_p skip_c kill
            | Flexible (n,s,e,min) ->
              let kill = "k_"^n in
              templateChannel kill;
              templateFlexible n min 0 false s start_c ee time_out ("skip_"^s) kill_p skip_c kill
            | _ -> ()
        ) l
      end
  in
  let rec splitIntervals intervals s =
    match intervals with
    | [] -> s
    | h::t ->
        let (_,a) = getEventsInterval h in
        let (new_s, new_intervals) = List.partition (fun x -> let (_,b) = getEventsInterval x in a = b) intervals in
        splitIntervals new_intervals (new_s::s)
  in
  let new_intervals = splitIntervals intervals [] in
  List.iter (fun x -> if (List.length x = 1) then createInterval (List.hd x) else createMergeIntervals x) new_intervals


(* Add intervals that are not defined in the score *)
let rec completeIntervals score intervals =
  (* Returns true if exists an interval which the event is the same that its event at the end *)
  let rec intervalPreExists intervals event =
    match intervals with
    | [] -> false
    | h::t -> let (s,e) = getEventsInterval h in
              if e = event then
                true
              else
                intervalPreExists t event
  in
  let addInterval start_event parent_event intervals  =
    if not (intervalPreExists intervals start_event) then
      begin
        let name = string_of_int (Random.bits()) in
        (* It has no an interaction point*)
        if ((isTimeEvent start_event) || (isCommandEvent start_event)) then
          Rigid ("i"^name, parent_event, start_event, getTimeEvent (getEvent start_event))::intervals
        else
          Flexible ("i"^name, parent_event, start_event, 0)::intervals
      end
    else
      intervals
  in
  let rec completeInterval p_event intervals t =
    match t with
    | Texture (n,s,e) -> addInterval s p_event intervals
    | Structure (n,s,e,l) ->
        List.fold_left (completeInterval s) (if p_event = "" then intervals else addInterval s p_event intervals) l
  in
  completeInterval "" intervals score


(* Return a list with the tos that determines the end of a structure *)
let getLastChildren children intervals =
  (* Returns true if exists an interval which the event is the same that its event at the start *)
  let rec intervalPreExists intervals event =
    match intervals with
    | [] -> false
    | h::t -> let (s,e) = getEventsInterval h in
              if s = event then
                true
              else
                intervalPreExists t event
  in
  let isLast intervals listChildren t =
    match t with
    | Texture (n,s,e) -> if (intervalPreExists intervals e) then listChildren else t::listChildren
    | Structure (n,s,e,l) -> if (intervalPreExists intervals e) then listChildren else t::listChildren
  in
  List.fold_left (isLast intervals) [] children

let createTemporalObjects intervals scenario =
  let getIntervals children_l intervals =
    List.fold_left
    (
      fun l c ->
        let interval = List.find_all (
         fun i ->
          let (si,ei) = getEventsInterval i in
          let (st,et) = getEventsTO c in
          (st = ei || et = si)
         )
        intervals in
        List.fold_left
        (fun a_l b_c ->
          if not (List.mem b_c a_l) then
            b_c::a_l
          else
            a_l)
        l interval
    )
    [] children_l
  in
  let rec createTemporalObject parent last_c event_c t =
    let kill_p = "k_"^parent in
    let isEndEvent = List.mem t last_c in
    let end_event e c p = if isEndEvent then p^c else e in
    match t with
    | Texture (n, s, e) ->
        let kill = "k_"^n in
        let skip = "skip_"^e in (* event sent by the point to the next temporal object *)
        let skip_p = "skip_"^s in
        templateChannel kill;
        templateChannel skip;
        (* texture without interaction point *)
        if (isTimeEvent e || isCommandEvent e ) then
          templateRigid n (getDuration s e) s (end_event e event_c "sc") skip_p kill_p (end_event skip event_c "skip_") kill
        (* texture with interaction point *)
        else
          begin
            let name_cond = string_of_int (Random.bits()) in
            let time_out = "t"^name_cond in
            let start_c = "scond"^name_cond in
            let (id_cond, id_cond_i) = getIdCondition e in
            let en_c = "en_"^id_cond in
            let skipped_cond = "skip_"^name_cond in
            templateChannel skipped_cond;
            templateChannel time_out;
            templateChannel start_c;
            templateVarBool en_c;
            templateFlexible n 0 0 false s start_c time_out  (end_event e event_c "sc") skip_p kill_p (end_event skip event_c "skip_") kill;
            (* TODO: define the skip events for the interactive point of a texture *)
            templateCondition name_cond id_cond_i en_c ("msg_"^id_cond) id_cond (isDefault e) start_c skip_p e kill_p skipped_cond time_out
          end
    | Structure (n, s, e, l) ->
        let last_children = getLastChildren l intervals in
        let interval_l = getIntervals l intervals in
        let time_out = "t"^(string_of_int (Random.bits())) in
        let kill = "k_"^n in
        let skip = "skip_"^e in (* event sent by the point to the next temporal object *)
        let skip_p = "skip_"^s in
        let skipped_c = "skip_"^(string_of_int (Random.bits())) in
        templateChannel skipped_c;
        templateChannel time_out;
        templateChannel kill;
        templateChannel skip;
        (* intervals *)
        Printf.printf "// Creating %d intervals ...\n" (List.length interval_l);
        createIntervals interval_l kill;
        (* structure without interaction point *)
        if (isTimeEvent e) then
          begin
            let name_c = string_of_int (Random.bits()) in
            let start_c = "sc"^name_c in
            let skip_c = "skip_"^name_c in
            templateChannel start_c;
            templateChannel skip_c;
            templateMix skip_p s kill skip_c; (* kill the children *)
            if (n = "Main") then
              templateFlexible n 0 0 false s start_c time_out (end_event e event_c "sc") skip_p kill_p (end_event skip event_c "skip_") kill
            else
              templateFlexible n (getDuration s e) 0 false s start_c time_out (end_event e event_c "sc") skip_p kill_p (end_event skip event_c "skip_") kill;
            (* TODO: Define the skip events for a structure without interactive points *)
            templateControl name_c ((List.length last_children)+1) start_c skip_c skipped_c time_out kill;
            List.iter (createTemporalObject n last_children name_c) l
          end
        (* structure with interaction point *)
        else
          begin
            let name_cond = string_of_int (Random.bits()) in
            let start_c = "scond"^name_cond in
            let (id_cond, id_cond_i) = getIdCondition e in
            let en_c = "en_"^id_cond in
            templateChannel start_c;
            templateVarBool en_c;
            templateFlexible n 0 0 false s start_c time_out e skip_p kill_p (end_event skip event_c "skip_") kill;
            templateCondition name_cond 0  en_c ("msg_"^id_cond) id_cond (isDefault e) start_c skip_p e kill_p skipped_c time_out;
            templateMix e (end_event skip event_c "skip_") kill_p kill; (* kill the children *)
            if isEndEvent then templateMix e ("skip_"^event_c) kill_p ("sc"^event_c); (* start control*)
            List.iter (createTemporalObject n [] "") l
          end
  in
  let parent = "scenario" in
  let skip = "skip_start" in
  templateChannel skip;
  templateChannel ("k_"^parent);
  createTemporalObject parent [] "" scenario


let createEvents events =
  (* event to start the scenario*)
  templateVarInt "msg_start";
  templateEvent "start" "msg_start" 0 1;
  (* external events *)
  Hashtbl.iter
  (
    fun k e ->
      match e with
      | ConditionalEvent (n,t,c) ->
          begin
            match c with
              | Some (cond,trigger,defa, i) ->
                  let msg = "msg_"^cond in
                  templateVarInt msg;
                  templateChannel cond;
                  templateEvent cond msg t 1
              | None -> ()
          end
      | _ -> ()
  )
  events


(* templateMix s_input skip kill s_output *)

(* templateFlexible name dmin dmax isFinite launch  min timeout stop skip_p kill_p skip kill  *)

(* templateRigid name duration launch  stop skip_p kill_p skip  *)

(* templateControl n_relations start_c skip_p skip launch kill *)

(* templateMix s_input skip s_output *)

(* templateCondition id_c external_e default launch skip_p stop kill_p skip start = *)

(* templateMix s_input skip s_output *)


(* =============================  Execution ========================================= *)
let complete_r = completeIntervals scenario relations
let _ = Printf.printf "// # of created intervals: %d\n" (List.length complete_r)
let _ = print_endline "// Creating channels ..."; createChannels events
let _ = print_endline "// Creating temporal objects ..."; createTemporalObjects complete_r scenario
let _ = print_endline "// Creating events ..."; createEvents events
