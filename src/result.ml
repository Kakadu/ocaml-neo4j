type ('a,'b) t = OK of 'a | Error of 'b

let (>>=) x f = match x with
  | OK a -> f a
  | Error b -> Error b

let (>>>) x f = match x with OK a -> OK (f a) | Error b -> Error b

