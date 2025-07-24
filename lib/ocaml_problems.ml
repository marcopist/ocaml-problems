(* Tail of a list*)
let rec last = function
  | [] -> None
  | [x] -> Some x
  | _ :: t -> last t

(* Last two elements of a list *)
let rec last_two = function
   | [] -> None
   | [x; y]-> Some ([x;y])
   | _ :: t -> last_two t