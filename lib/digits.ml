open Core

type d = { positive : bool; digits : int list }

let zero = { positive = true; digits = [] }
let one = { positive = true; digits = [ 1 ] }

let remove_leading_zeros l =
  let rec aux = function [] -> [] | 0 :: t -> aux t | l -> l in
  aux l

let of_int n =
  let positive = n >= 0 in
  let rec to_digits x =
    if x = 0 then [] else Int.abs (x % 10) :: to_digits (x / 10)
  in
  let digits = if n = 0 then [] else to_digits n in
  { positive; digits }

let to_int d =
  let abs_val, _ =
    List.fold d.digits ~init:(0, 1) ~f:(fun (acc, mult) x ->
        (acc + (x * mult), mult * 10))
  in
  if d.positive then abs_val else -abs_val

let to_list d = List.rev d.digits

let compare_magnitudes d1 d2 =
  let len1 = List.length d1 in
  let len2 = List.length d2 in
  if len1 > len2 then 1
  else if len1 < len2 then -1
  else List.compare Int.compare (List.rev d1) (List.rev d2)

let add_lists l1 l2 =
  let rec aux c l1 l2 acc =
    match (l1, l2) with
    | [], [] -> if c = 0 then acc else c :: acc
    | [], x :: xs | x :: xs, [] ->
        let s = x + c in
        aux (s / 10) xs [] ((s % 10) :: acc)
    | x :: xs, y :: ys ->
        let s = x + y + c in
        aux (s / 10) xs ys ((s % 10) :: acc)
  in
  List.rev (aux 0 l1 l2 [])

let sub_lists l1 l2 =
  let rec aux c l1 l2 acc =
    match (l1, l2) with
    | [], [] -> acc
    | [], _ -> failwith "l1 < l2 in sub_lists"
    | x :: xs, [] ->
        let d = x - c in
        if d < 0 then aux 1 xs [] ((d + 10) :: acc) else aux 0 xs [] (d :: acc)
    | x :: xs, y :: ys ->
        let d = x - y - c in
        if d < 0 then aux 1 xs ys ((d + 10) :: acc) else aux 0 xs ys (d :: acc)
  in
  List.rev (remove_leading_zeros (aux 0 l1 l2 []))

let mul_scalar l s =
  if s = 0 then []
  else if s = 1 then l
  else
    let rec aux c l acc =
      match l with
      | [] ->
          let rec flush c acc =
            if c = 0 then acc else flush (c / 10) ((c % 10) :: acc)
          in
          flush c acc
      | x :: xs ->
          let v = (x * s) + c in
          aux (v / 10) xs ((v % 10) :: acc)
    in
    List.rev (aux 0 l [])

let mul_lists l1 l2 =
  let rec prepend_zeros n l =
    if n = 0 then l else prepend_zeros (n - 1) (0 :: l)
  in
  let rec aux l2_digits shift acc =
    match l2_digits with
    | [] -> acc
    | d :: rest ->
        let term = mul_scalar l1 d in
        (* Shift in LE means prepending zeros *)
        let shifted = prepend_zeros shift term in
        aux rest (shift + 1) (add_lists acc shifted)
  in
  aux l2 0 []

let neg d = { d with positive = not d.positive }

let sum d1 d2 =
  if Bool.equal d1.positive d2.positive then
    { positive = d1.positive; digits = add_lists d1.digits d2.digits }
  else
    let cmp = compare_magnitudes d1.digits d2.digits in
    if cmp = 0 then zero
    else if cmp > 0 then
      { positive = d1.positive; digits = sub_lists d1.digits d2.digits }
    else { positive = d2.positive; digits = sub_lists d2.digits d1.digits }

let diff d1 d2 = sum d1 (neg d2)

let prod d1 d2 =
  let digits = mul_lists d1.digits d2.digits in
  if List.is_empty digits then zero
  else { positive = Bool.equal d1.positive d2.positive; digits }

let ( + ) = sum
let ( - ) = diff
let ( * ) = prod
