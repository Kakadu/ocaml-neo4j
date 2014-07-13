open Printf
open Helpers
open Http_client.Convenience
open Result

let http_get  = Http_client.Convenience.http_get
let http_post = Http_client.Convenience.http_post
let to_json = Yojson.Safe.from_string
let print_json = Yojson.Safe.pretty_to_channel stdout

module YoUtil = struct
  let drop_assoc = function `Assoc xs -> xs | _ -> failwith "Bad argument"
  let drop_string = function `String s -> s | _ -> failwith "Bad argument"
  let unwrap_res x = x |> drop_assoc |> List.assoc "data"
end

type options = {
  server: string;
  port : int;
}

module type CONFIG = sig
    val server : string
    val port : int
end

module Make(Cfg: CONFIG) = struct


let make_empty_node () : (_,_) Result.t =
  let url = sprintf "http://%s:%d/db/data/node/" Cfg.server Cfg.port in
  let node_properties : (string*string) list = [] in
  let s = (http_post_message url node_properties)#get_resp_body () in
  (match to_json s with
  | `Assoc xs -> `OK (List.Assoc.find_exn xs "self")
  | _ -> `Error ()) >>= fun url ->
   `OK (int_of_string @@ String.rsplit (Yojson.Safe.to_string url) ~by:'/'  )



let get_node ?(server= (!options).server) ?(port = !options.port) nodeid =
  Http_client.Convenience.http_get @@ sprintf "http://%s:%d/db/data/node/%d/" server port nodeid

let node_properties ?(server= (!options).server) ?(port = !options.port) nodeid =
  http_get @@ sprintf "http://%s:%d/db/data/node/%d/properties" server port nodeid

let labels ?(server= (!options).server) ?(port = !options.port) () =
  http_get @@ sprintf "http://%s:%d/db/data/labels/" server port

let add_label id label =
  let url = sprintf "http://%s:%d/db/data/node/%d/labels" !options.server !options.port id in
  let pipeline = new Http_client.pipeline in (*
  let opt = pipeline # get_options in
  pipeline # set_options
    { opt with
        Http_client.number_of_parallel_connections = 1;
        Http_client.verbose_status = true;
        Http_client.verbose_connection = true;
        Http_client.verbose_response_header = true;
        Http_client.verbose_response_contents = true;
        Http_client.verbose_request_header = true;
        Http_client.verbose_request_contents = true;  };
*)
  let req = new Http_client.post_raw url (sprintf "\"%s\"" label) in
  req#set_req_header "Accept"       "application/json; charset=UTF8";
  req#set_req_header "Content-type" "application/json";
  pipeline#add req;
  pipeline#run ()

type cypher_msg = string
let string_of_cypher_msg (x:cypher_msg) = x

let post_cypher ?(server= (!options).server) ?(port = !options.port) ?(params=[]) cypher =
  let url = sprintf "http://%s:%d/db/data/cypher" server port in
  let pipeline = new Http_client.pipeline in
  let opt = pipeline # get_options in
  pipeline # set_options
    { opt with
        Http_client.number_of_parallel_connections = 1;
        Http_client.verbose_status = true;
        Http_client.verbose_connection = true;
        Http_client.verbose_response_header = true;
        Http_client.verbose_response_contents = true;
        Http_client.verbose_request_header = true;
        Http_client.verbose_request_contents = true;  };
  let args = `Assoc [ ("query", `String cypher);
                      ("params", `Assoc params) ] in (*
               (List.Labels.map params ~f:(fun (name,j) -> (name,Yojson.to_string j))) in
                                          *)
  (*printf "Args: %s\n%!" (Yojson.to_string args);*)
  print_endline @@ Str.global_replace (Str.regexp "\n") " " cypher;
  let req = new Http_client.post_raw url (Yojson.to_string args) in
  (*
  let req = new Http_client.post url
                [("query",cypher);("params", Yojson.to_string (`Assoc params))]in
  *)
  req#set_req_header "Accept"       "application/json; charset=UTF8";
  req#set_req_header "Content-type" "application/json";
  pipeline#add_with_callback req (fun _ -> print_endline "callback");
  pipeline#run ();
  req#get_resp_body ()


let remove_all () : (_,_) Result.t =
  let good_r = `Assoc [("columns", `List []); ("data", `List [])] in
  let j2 = to_json @@ post_cypher "START r=rel(*)  DELETE r;" in
  let j1 = to_json @@ post_cypher "START n=node(*) DELETE n;" in
  if good_r = j1 && good_r = j2
  then `OK ()
  else `Error ()

let insert_node_between id1  =
  let cmd = sprintf
	"START n=node(%d)
         MATCH n-[r:FOLLOWED_BY]->m
         DELETE r
	 CREATE UNIQUE n-[r1:FOLLOWED_BY]->(k{title:'qwe'})-[r2:FOLLOWED_BY]->m
	 set k: TIMELINE_ITEM
	" id1
  in
  print_endline (Str.global_replace (Str.regexp "\n") cmd " ");
  post_cypher cmd


class node_of_json (j: (string * Yojson.Safe.json) list) = object
  method id : int =
    match List.assoc "self" j with
    | `String s -> int_of_string @@ String.rsplit s ~by:'/'
    | _ -> failwith "Wrong json"
  method json = j
  method data = List.assoc "data" j
  method prop name = List.assoc name j
end

class date_of_json (j: (string * Yojson.Safe.json) list) = object
  method id : int =
    match List.assoc "self" j with
    | `String s -> int_of_string @@ String.rsplit s ~by:'/'
    | _ -> failwith "Wrong json"
  method json = j
  method data = List.assoc "data" j
  method when_ = match List.assoc "data" j with
    | `Assoc xs -> List.assoc "when" xs |> (function `String s -> s | _ -> assert false)
    | _ -> assert false
end

class question_of_json (j: (string * Yojson.Safe.json) list) = object(self)
  method id : int =
    match List.assoc "self" j with
    | `String s -> int_of_string @@ String.rsplit s ~by:'/'
    | _ -> failwith "Wrong json"
  method json = j
  method data = List.assoc "data" j
  method prop name = match List.assoc "data" j with
    | `Assoc xs -> List.assoc name xs |> (function `String s -> s | _ -> assert false)
    | _ -> assert false
  method text = self#prop "text"
end
(*
let get_start_day () =
  ukr_start_node () >>= fun start_node_id ->
  let cmd = sprintf
              "START x=node(%d) MATCH d<-[:WHEN]-x RETURN d;"
              start_node_id
  in
  let ans = post_cypher (*~params:["first_id", `Int start_node_id]*) cmd in
  (*print_endline ans;*)
  match to_json ans with
  | `Assoc [_; ("data",`List [`List [`Assoc xs]])] -> `OK (new date_of_json xs)
  |  _ -> `Error "JSON match failure"
 *)
let get_questions nodeid =
  let cmd = sprintf "START x=node(%d)
                     MATCH x-[:HAS_QUESTION]->y RETURN y" nodeid in
  let j = to_json @@ post_cypher cmd |> YoUtil.drop_assoc |> List.assoc "data" in
  print_endline @@ Yojson.Safe.to_string j;
  match j with
  | `List[`List ys ] -> `OK (List.map (fun y -> new question_of_json (YoUtil.drop_assoc y)) ys)
  | _ -> `Error "JSON match failure"

let get_next_timeline_node nodeid =
  let cmd = sprintf "START x=node(%d)
                     MATCH x-[:FOLLOWED_BY]->y RETURN y" nodeid in
  match to_json @@ post_cypher cmd |> YoUtil.drop_assoc |> List.assoc "data" with
  | `List[`List[`Assoc xs]] -> `OK (new node_of_json xs)
  | _ -> `Error "JSON match failure"

let id_from_node_json ej =
  match List.Assoc.find_exn ej "self" with
  | `String s -> Int64.of_string @@ String.rsplit s ~by:'/'
  | _ -> failwith "Wrong json for function id_from_node_json"

end
