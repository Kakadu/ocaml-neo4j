open Printf

let string_of_char = String.make 1
let failwiths fmt = failwith (sprintf fmt)

module Result = Result
type ('a,'b) result = ('a,'b) Result.t

module Option = struct
  let map ~f = function Some x -> Some (f x) | None -> None
  let value ~f ~default = function Some x -> f x | None -> default
  let get ~default = function Some x -> x | None -> default
  let get_exn = function Some x -> x | None -> failwith "Bad argument of get_exn"
  let is_some = function Some _ -> true | None  -> false
  let is_none = function Some _ -> false | None  -> true
  let iter ~f = function Some x -> f x | None -> ()
end

module List = struct
  include ListLabels
  module Assoc = struct
    let find_exn xs x = assoc x xs
    let find     xs x = try Some (find_exn xs x) with Not_found  -> None
  end
  let hd_exn = function
    | x::_ -> x
    | [] -> failwith "Bad argument of hd_exn"
  let tl_exn = function
    | x::_ -> x
    | [] -> failwith "Bad argument of tl_exn"
  let rec last_exn = function
    | [x] -> x
    | x::xs -> last_exn xs
    | [] -> failwith "Bad argument of last_exn"

  let find_exn = find
  let find ~f xs =
    try Some (find_exn ~f xs)
    with Not_found -> None

  let map ~f xs = List.map f xs
  let filter_map ~f xs =
    fold_left ~init:[] ~f:(fun acc x -> match f x with Some y -> y::acc | None -> acc) xs

  let concat_strings ~sep ~f xs =
    let b = Buffer.create 100 in
    iter xs ~f:(fun item -> Buffer.add_string b (f item); Buffer.add_char b sep);
    Buffer.contents b

  let to_string ~f xs =
    sprintf "(%s)" (concat_strings ~sep:' ' ~f xs)

end

module String = struct
  include StringLabels
  let split ~on s = Str.(split (regexp @@ string_of_char on) s)
  let split_s ~on s = Str.(split (regexp on) s)
  let rsplit ~by s =
    try
      let i = rindex s by in
      sub s ~pos:(i+1) ~len:(String.length s - i - 1)
    with Not_found -> s
end

module Int = struct
  let compare: int -> int -> int = Pervasives.compare
  let max a b = if compare a b > 0 then a else b
end

module Exn = struct
  let to_string = Printexc.to_string
  let backtrace = Printexc.get_backtrace
end


