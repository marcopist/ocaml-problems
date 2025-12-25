[@@@warning "-32-33"]

open Seq

let sorry () = failwith "not implemented"

type d = bool * int list

let of_int n =
  let rec aux acc n =
    if n < 10 then n :: acc else aux ((n mod 10) :: acc) (n / 10)
  in
  match n >= 0 with
  | true -> (true, aux [] n)
  | false -> (false, aux [] (-n))

let to_int d =
  let rec aux acc = function
    | [] -> acc
    | h :: t -> aux ((acc * 10) + h) t
  in
  aux 0 d

let to_list d = d

type order = Bigger | Equal | Smaller

let compare a b =
  match a - b with 0 -> Equal | _ -> if a > b then Bigger else Smaller

let rec pad dig n =
  if List.length dig >= n then dig
  else pad (0 :: dig) n

let sum d1 d2 =
  let n = max (List.length d1) (List.length d2) in
  let d1 = pad d1 n in
  let d2 = pad d2 n in
  List.map2 ( + ) d1 d2

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
