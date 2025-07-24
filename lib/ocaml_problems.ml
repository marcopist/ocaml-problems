module type Problem = sig
  val result : string
end

module Problem1 : Problem = struct
  let rec generator n =
    match n with
    | 0 -> 0
    | _ ->
        let rest = generator (n - 1) in
        if n mod 5 = 0 || n mod 3 = 0 then n + rest else rest

  let result = string_of_int (generator 999)
end
