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

let ( -- ) a b =
  let rec aux i acc = if i = a then i :: acc else aux (i - 1) (i :: acc) in
  aux b []

let cartesian l l' =
  List.concat @@ List.map (fun e -> List.map (fun e' -> (e, e')) l') l
