open Seq

let rec digits n =
  let last_digit = n mod 10 in
  let div = n / 10 in
  match div with 0 -> [ last_digit ] | _ -> last_digit :: digits div

let rec num_digits n =
  let next = n / 10 in
  if next = 0 then 1 else num_digits next + 1

let rec pow n exponent = if exponent = 0 then 1 else n * pow n (exponent - 1)
let digit_at n pos = n / pow 10 (pos - 1) mod 10
let last_digit n = n mod 10

(* This is O(n) space and time. I'm aware there is an O(n/2) space solution. TODO: Implement it. *)
let is_palindrome_list x = x = List.rev x
let is_palindrome n = is_palindrome_list @@ digits n

(* Returns (k, prime_factor) where prime_factor is the smallest prime factor of n and n = k * prime_factor *)
let factorise_once n =
  let rec factorise_once_impl n current stop_at =
    if current > stop_at then None
    else if n mod current = 0 then Some (n / current, current)
    else factorise_once_impl n (current + 1) stop_at
  in
  factorise_once_impl n 2 (int_of_float @@ sqrt @@ float_of_int n)

let rec prime_factorise n =
  let first_factorisation = factorise_once n in
  match first_factorisation with
  | None -> [ n ]
  | Some (f1, prime_factor) -> prime_factor :: prime_factorise f1

let rec multiplicity factor n =
  if n mod factor = 0 then 1 + multiplicity factor (n / factor) else 0

let rec count_factors n =
  if n = 1 then 1
  else
    let first_factorisation = factorise_once n in
    match first_factorisation with
    | None -> 2 (* n is prime, so [1, n] *)
    | Some (_f, prime_factor) ->
        let m = multiplicity prime_factor n in
        (m + 1) * count_factors (n / pow prime_factor m)

let ( -- ) a b =
  let rec aux i acc = if i = a then i :: acc else aux (i - 1) (i :: acc) in
  aux b []

let cartesian l l' =
  List.concat @@ List.map (fun e -> List.map (fun e' -> (e, e')) l') l

let rec factorial = function 1 -> 1 | n -> n * factorial (n - 1)
let rec choose n m = match m with 0 -> 1 | _ -> n * choose (n - 1) (m - 1) / m
let triangle_numbers = scan ( + ) 1 (ints 2)

let rec handle_carry digs =
  let carry = List.map (fun x -> max (x - 9) 0) digs in
  let shifted_carry = carry @ [ 0 ] in
  match List.filter (fun x -> x > 0) carry with
  | [] -> digs
  | _ -> (
      let padded_digs = 0 :: digs in
      let handled_sum = List.map2 ( + ) padded_digs shifted_carry in
      match handled_sum with
      | 0 :: rest -> handle_carry rest
      | _ -> handle_carry handled_sum)

let pad digits1 digits2 =
  let diff = List.length digits1 - List.length digits2 in

  let list1 =
    ([ 0 ] |> List.to_seq |> Seq.cycle |> Seq.take @@ max 0 diff |> List.of_seq)
    @ digits1
  in
  let list2 =
    ([ 0 ] |> List.to_seq |> Seq.cycle
    |> Seq.take @@ max 0 (-diff)
    |> List.of_seq)
    @ digits2
  in

  (list1, list2)

let rec unpad = function 0 :: rest -> unpad rest | x -> x
let digits_sum d1 d2 = List.map2 ( + ) d1 d2 |> handle_carry
let digs_times_number digs number = handle_carry @@ List.map (( * ) number) digs

let rec carry a =
  match a with
  | [] -> []
  | [ a ] ->
      let divisor = a / 10 in
      if divisor = 0 then [ a ] else (a mod 10) :: carry [ divisor ]
  | a :: b :: rest -> (
      let divisor = a / 10 in
      match divisor with
      | 0 -> a :: carry (b :: rest)
      | x ->
          let d = a mod 10 in
          d :: (carry @@ ((b + x) :: rest)))
