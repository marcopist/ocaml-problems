let rec digits n =
  let last_digit = n mod 10 in
  let div = n / 10 in
  match div with 0 -> [ last_digit ] | _ -> last_digit :: digits div

let is_palindrome n =
  let n_digits = digits n in
  List.rev n_digits = n_digits (* TODO: This is unnecessarily slow *)
