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

type 'a event =
    { event_desc: 'a; when_: Calendar.t; event_title: string
    ; event_url: string
    }

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

    let (_ : (_,_) result) = day_node_id >>= fun day_node_id ->
      create_event ~parentid:day_node_id event_desc >>= fun _ ->
      OK ()
    in
    ()
  in
  List.iter events ~f

let events =
  (* TODO maybe add variable with level of fakeness *)
  let open Calendar in
  [ { when_ = make 2014 07 17 19 02 00
    ; event_title="Ukraine relocates SA-17 Grizzly to Ukrainian-Russian border"
    ; event_desc=""
    ; event_url ="http://anti-maidan.com/index.php?p=news&id=3957"
    }
  ; { when_ = make 2014 07 17 20 00 00
    ; event_title="Boeing 777 of Malaisia airlaines crashes near Ukrainian-Russian border"
    ; event_desc=""
    ; event_url= "" }
  ; { when_ = make 2014 07 17 22 00 00
    ; event_title="Mass-Media: The real target of Ukranian missle was V.Putin's airplane"
    ; event_desc=""
    ; event_url= "http://anti-maidan.com/index.php?p=news&id=3965" }
  ; { when_ = make 2014 07 17 22 19 00
    ; event_title="Flight recorders are found on planecrash area"
    ; event_desc=""
    ; event_url= "http://anti-maidan.com/index.php?p=news&id=3966" }
  ; { when_ = make 2014 07 17 22 51 00
    ; event_title="Flightradar24: before its dissappearing Boeing was located near city of Kremenchug"
    ; event_desc=""
    ; event_url= "http://anti-maidan.com/index.php?p=news&id=3968" }
  ; { when_ = make 2014 07 17 23 20 00
    ; event_title="CNN: Are separatists able to destrot Boeing?"
    ; event_desc=""
    ; event_url= "http://anti-maidan.com/index.php?p=news&id=3967" }
  ; { when_ = make 2014 07 18 00 13 00
    ; event_title="V.Putin accuses Ukraine in Boeing catastophe"
    ; event_desc=""
    ; event_url= "http://anti-maidan.com/index.php?p=news&id=3971" }
  ; { when_ = make 2014 07 18 00 25 00
    ; event_title="Donetsk People's Republic is concluding local armistice near plane's crash area"
    ; event_desc=""
    ; event_url= "http://anti-maidan.com/index.php?p=news&id=3973" }
  ; { when_ = make 2014 07 18 01 14 00
    ; event_title="Spanish dispatcher have seen Ukrainian Air Forces near crashed Boeing"
    ; event_desc=""
    ; event_url= "http://anti-maidan.com/index.php?p=news&id=3976" }
  ; { when_ = make 2014 07 18 08 15 00
    ; event_title="OSCE is calling to seal off zone of aircrash"
    ; event_desc=""
    ; event_url= "http://anti-maidan.com/index.php?p=news&id=3994" }
  ; { when_ = make 2014 07 18 14 14 00
    ; event_title="DNR: Kiev's attempts to claim us in Boeing catastrophe are awkward"
    ; event_desc=""
    ; event_url= "http://anti-maidan.com/index.php?p=news&id=3988" }
  ; { when_ = make 2014 07 18 20 37 00
    ; event_title="OSCE wathers have got limited access to the area of aircrash"
    ; event_desc=""
    ; event_url= "http://anti-maidan.com/index.php?p=news&id=4009" }
  ; { when_ = make 2014 07 18 21 48 00
    ; event_title="John Kirby: The Pentagon doesn't know who have destroyed the Boeing MH17"
    ; event_desc=""
    ; event_url= "http://anti-maidan.com/index.php?p=news&id=4011" }
  ; { when_ = make 2014 07 18 21 55 00
    ; event_title="Journalist: Boeing was flying at 480 km to north comparately to normal route"
    ; event_desc=""
    ; event_url= "http://anti-maidan.com/index.php?p=news&id=4013" }
  ; { when_ = make 2014 07 19 04 43 00
    ; event_title="Karakas: The catastrophe of Boeing is result of actions of USA"
    ; event_desc=""
    ; event_url= "http://anti-maidan.com/index.php?p=news&id=4025" }
  ; { when_ = make 2014 07 19 06 38 00
    ; event_title="S.Lavrov: Russia will not decypher 'black boxes' on its own territory"
    ; event_desc=""
    ; event_url= "http://anti-maidan.com/index.php?p=news&id=4028" }
  ; { when_ = make 2014 07 19 07 10 00
    ; event_title="China and Argentina call to objective investigation of aircrash"
    ; event_desc=""
    ; event_url= "http://anti-maidan.com/index.php?p=news&id=4030" }
  ; { when_ = make 2014 07 19 23 27 00
    ; event_title="Ministry of Defense of Russian asks 10 questions about Boeing to Kiev"
    ; event_desc=""
    ; event_url= "http://anti-maidan.com/index.php?p=news&id=4038" }
  ; { when_ = make 2014 07 19 20 00 00
    ; event_title=""
    ; event_desc=""
    ; event_url= "" }
  ; { when_ = make 2014 07 19 20 00 00
    ; event_title=""
    ; event_desc=""
    ; event_url= "" }
  ;



    { event_desc="event1"; when_ = make 2009 08 23 15 32 43
    ; event_title=""; event_url= "" }
  ; { event_desc="event2"; when_ = make 2009 09 22 15 32 43
    ; event_title=""; event_url= "" }
  ; { event_desc="event3"; when_ = make 2009 09 23 15 32 43
    ; event_title=""; event_url= "" }
  ; { event_desc="event4"; when_ = make 2009 09 23 16 11 04
    ; event_title=""; event_url= "" }
  ; { event_desc="event5"; when_ = make 2012 10 21 12 13 14
    ; event_title=""; event_url= "" }
  ]

let () =
  make_nodes events

























