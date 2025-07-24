open Ocaml_problems
open Digest

(* Create a list of module-hash pairs *)
let problem_hashes =
  [
    ( "Problem 1",
      (module Problem1 : Problem),
      "e1edf9d1967ca96767dcc2b2d6df69f4" );
    (* Add more problems here as you create them *)
    (* ("Problem2", (module Problem2 : Problem), "expected_hash_2"); *)
  ]

(* Generic test function *)
let test_problem name problem_module expected_hash =
  let module P = (val problem_module : Problem) in
  let actual_hash = MD5.to_hex (MD5.string P.result) in
  let error_msg = Printf.sprintf "%s hash (result: %s)" name P.result in
  Alcotest.(check string) error_msg expected_hash actual_hash

(* Generate tests for all problems *)
let generate_tests () =
  List.map
    (fun (name, problem_module, expected_hash) ->
      Alcotest.test_case name `Quick (fun () ->
          test_problem name problem_module expected_hash))
    problem_hashes

let () =
  let open Alcotest in
  run "Problem Tests" [ ("hash_tests", generate_tests ()) ]
