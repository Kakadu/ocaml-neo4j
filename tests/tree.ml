open Helpers
open Neorest
open CalendarLib
open Printf

(*
let () = Http_client.Convenience.http_verbose ~verbose_status:true ()
 *)
type 'a event = { event_desc: 'a; when_: Calendar.t }

module API = Neorest.Make(struct let server="localhost" let port=7474 end)

let make_nodes events =
  let () = match API.remove_all () with
    | `OK () -> ()
    | `Error () -> fprintf stderr "Can't connect to database"; exit 1
  in
  let f = fun {event_desc; when_} ->
    let cmd = sprintf
                "CREATE
                 (:YEAR {n:{yy}}) -[:MONTH]->
                 (:MONTH{n:{mm}}) -[:DAY]->
                 (:DAY  {n:{dd}}) -[:EVENT]->
                 (:EVENT{desc: {desc} })"
    in
    let params = [ ("yy", `Int (Calendar.year  when_))
                 ; ("mm", `Int (Calendar.month when_ |> Date.int_of_month))
                 ; ("dd", `Int (Calendar.day_of_month when_))
                 ; ("desc", `String (Printer.CalendarPrinter.to_string when_))
                 ]
    in
    let (_: string) =  API.post_cypher ~params cmd in
    ()
  in
  List.iter events ~f

let events =
  let open Calendar in
  [ { event_desc="event1"; when_ = make 2009 08 23 15 32 43 }
  ]

let () =
  make_nodes events

