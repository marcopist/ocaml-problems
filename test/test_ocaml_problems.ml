open Ocaml_problems

(* Test utilities *)
let test_last_option =
  Alcotest.testable (Fmt.option Fmt.int) (Option.equal Int.equal)

let test_last_string_option =
  Alcotest.testable (Fmt.option Fmt.string) (Option.equal String.equal)

let test_last_bool_option =
  Alcotest.testable (Fmt.option Fmt.bool) (Option.equal Bool.equal)

(* Test cases for the last function *)
let test_last_empty_list () =
  Alcotest.(check test_last_option) "last of empty list" None (last [])

let test_last_single_element () =
  Alcotest.(check test_last_option) "last of single int" (Some 1) (last [ 1 ]);
  Alcotest.(check test_last_string_option)
    "last of single string" (Some "hello") (last [ "hello" ])

let test_last_multiple_elements () =
  Alcotest.(check test_last_option)
    "last of multiple ints" (Some 3)
    (last [ 1; 2; 3 ]);
  Alcotest.(check test_last_string_option)
    "last of multiple strings" (Some "c")
    (last [ "a"; "b"; "c" ]);
  Alcotest.(check test_last_bool_option)
    "last of multiple bools" (Some false)
    (last [ true; false; true; false ])

let test_last_two_elements () =
  Alcotest.(check test_last_option) "last of two ints" (Some 2) (last [ 1; 2 ]);
  Alcotest.(check test_last_string_option)
    "last of two strings" (Some "second")
    (last [ "first"; "second" ])

(* Test suite definition *)
let last_tests =
  [
    ("empty list", `Quick, test_last_empty_list);
    ("single element", `Quick, test_last_single_element);
    ("multiple elements", `Quick, test_last_multiple_elements);
    ("two elements", `Quick, test_last_two_elements);
  ]

(* Main test runner *)
let () = Alcotest.run "OCaml Problems Tests" [ ("last function", last_tests) ]
