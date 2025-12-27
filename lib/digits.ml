[@@@warning "-32-33"]

open Seq
open Core

let sorry () = failwith "not implemented"

type d = { positive : bool; digits : int list }

let rec carry_one x =
  if x = 0 then []
  else
    let divisor = x / 10 in
    match divisor with 0 -> [ x ] | r -> carry_one r @ [ x mod 10 ]

let of_int (n : int) : d =
  let positive = n >= 0 in
  let abs = if positive then n else -n in
  let digits = carry_one abs in
  { positive; digits }

let rec ipow a = function
  | 0 -> 1
  | 1 -> a
  | n ->
    let b = ipow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a)

let to_int (d: d) : int =
  let abs = List.foldi d.digits ~init:0  ~f:(fun i acc x -> acc + (ipow i 10) * x ) in
  if d.positive then abs else -abs

let carry d =
  let digits = match d.digits with
  | [] -> []
  | [0] -> []
  | 
  0

let to_list d = d

let compare a b =
  match a - b with
  | 0 -> Ordering.Equal
  | _ -> if a > b then Ordering.Greater else Ordering.Less

let rec pad dig n = if List.length dig >= n then dig else pad (0 :: dig) n
let sum d1 d2 = []
let scale s d = List.map (fun di -> di * s) d
let shift d = 0 :: d
let diff d1 d2 = sum d1 (scale (-1) d2)

[@@@warning "-27"]

let carry d = sorry ()

let prod d1 d2 =
  let rec aux d1 d2 =
    match d2 with
    | [] -> [ 0 ]
    | [ c ] -> scale c d1
    | c :: rest -> sum (scale (c * 10) d1) (aux d1 rest)
  in
  carry @@ aux d1 d2

let zero = of_int 0
let one = of_int 1
let ( + ) = sum
let ( - ) = diff
let neg = scale (-1)
let ( * ) = prod
