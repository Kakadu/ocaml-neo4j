open Helpers
open Helpers.Result
open Neorest
open CalendarLib
open Printf

(*  Creating timeline
 *
 *
 *  ----2014-01-13--------------2014-02-14--------------> .....
 *      /     |    \            /    |    \
 *  event1 event2   event3   event4 event5   event6
 *
 *)

type 'a event = { event_desc: 'a; when_: Calendar.t }

module API = Neorest.Make(struct let server="localhost" let port=7474 end)

let make_nodes events =
  let () = match API.remove_all () with
    | OK () -> ()
    | Error () -> fprintf stderr "Can't connect to database"; exit 1
  in

  let has_date ts =
    let cmd = "OPTIONAL MATCH (d:DAY) WHERE d.timestamp={ts} RETURN id(d)" in
    let params = [ "ts", `Int ts ] in
    API.wrap_cypher cmd ~params ~f:(function
    | `List [`Int id ] -> OK (Some id)
    | `List [ `List [`Null] ] -> OK None
    | _ -> Error ""
    )
  in
  let get_prev ts =
    let cmd = "MATCH (e:DAY) WHERE e.timestamp < {ts}
               RETURN e.timestamp,id(e) ORDER BY e.timestamp DESC LIMIT 1
               "
    in
    let params = [ "ts", `Int ts ] in
    let (ans: string) = API.post_cypher ~params cmd in
    print_endline ans;
    match to_json ans |> YoUtil.drop_assoc |> List.assoc "data" with
    | `List[]  -> OK None
    | `List xs when List.length xs > 1  -> Error "Too many results"
    | `List[`List[ `Int _ts; `Int id] ] ->  OK (Some (_ts,id) )
    | _ -> Error "Wrong format"
  in
  let get_next ts =
    let cmd = "MATCH (e:DAY) WHERE e.timestamp > {ts}
               RETURN e.timestamp,id(e) ORDER BY e.timestamp LIMIT 1
               "
    in
    let params = [ "ts", `Int ts ] in
    let (ans: string) = API.post_cypher ~params cmd in
    print_endline ans;
    match to_json ans |> YoUtil.drop_assoc |> List.assoc "data" with
    | `List[]  -> OK None
    | `List xs when List.length xs > 1  -> Error "Too many results"
    | `List[`List[ `Int _ts; `Int id] ] ->  OK (Some (_ts,id) )
    | _ -> Error "Wrong format"
  in
  let create_day when_ =
    let ts = Calendar.to_date when_ |> Date.to_unixfloat |> int_of_float in
    let params = [ ("ts", `Int ts); ("desc", `String (when_ |> Calendar.to_date |> Printer.Date.to_string)) ] in
    let cmd = "MERGE (ans:DAY{ timestamp: {ts}, desc: {desc} }) RETURN id(ans)" in
    API.wrap_cypher cmd ~params ~f:(function
    | `List[ `List[ `Int id ] ] -> OK id
    | _ -> Error ""
    )
  in
  let create_daylink ~from ~dest =
    let params = [ ("from", `Int from); ("dest", `Int dest) ] in
    let cmd = "START l=node({from}), r=node({dest}) MERGE l-[:NEXT_DAY]->r" in
    let _ = API.post_cypher cmd ~params in
    OK ()
  in
  let connect_days = create_daylink in

  let create_event ~parentid desc =
    let params = [ ("parent", `Int parentid); ("desc", `String desc) ] in
    let cmd = "START day=node({parent})
               CREATE day-[:HAS_EVENT]->(e:EVENT{desc: {desc} })
               RETURN id(e)
              " in
    API.wrap_cypher cmd ~params ~f:(fun _ ->
      OK ()
    )
  in
  let f = fun {event_desc; when_} ->
    let day_ts = Calendar.to_date when_ |> Date.to_unixfloat |> int_of_float in
    let day_node_id : (int,_) result =
      has_date day_ts >>= function
      | None -> begin
          get_prev day_ts >>= fun prev_info ->
          get_next day_ts >>= fun next_info ->
          match prev_info,next_info with
            | None,None -> (* 1st node *)
              print_endline "Its a 1st node";
              create_day when_ >>= fun _newid ->
              OK _newid
            | Some (prev_ts,prev_id), None ->
              (* We need to establish link between previous node and current one *)
              print_endline "There is prev. No next";
              create_day when_ >>= fun new_id ->
              connect_days ~from:prev_id ~dest:new_id >>= fun () ->
              OK new_id
            | None,Some (next_ts,next_id) ->
              (* We need to establish link between previous node and current one *)
              create_day when_ >>= fun new_id ->
              connect_days ~from:new_id ~dest:next_id >>= fun () ->
              OK new_id
            | Some (prev_ts,prev_id), Some (next_ts,next_id) ->
              (* We need to establish link between previous node and current one *)
              create_day when_ >>= fun new_id ->
              connect_days ~from:new_id  ~dest:next_id >>= fun () ->
              connect_days ~from:prev_id ~dest:new_id  >>= fun () ->
              OK new_id
        end
      | Some id -> (* Date node already created. Do nothing *)
        OK id
    in

    let _ : (_,_) result = day_node_id >>= fun day_node_id ->
      create_event ~parentid:day_node_id event_desc >>= fun _ ->
      OK ()
    in
    ()
  in
  List.iter events ~f

let events =
  let open Calendar in
  [ { event_desc="event1"; when_ = make 2009 08 23 15 32 43 }
  ; { event_desc="event2"; when_ = make 2009 09 22 15 32 43 }
  ; { event_desc="event3"; when_ = make 2009 09 23 15 32 43 }
  ; { event_desc="event4"; when_ = make 2009 09 23 16 11 04 }
  ; { event_desc="event5"; when_ = make 2012 10 21 12 13 14 }
  ;
  ]

let () =
  make_nodes events

























