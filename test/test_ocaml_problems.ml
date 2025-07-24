open Ocaml_problems

let test_last_empty_list () =
  assert (last [] = None)

let test_last_single_element () =
  assert (last [1] = Some 1);
  assert (last ["hello"] = Some "hello")

let test_last_multiple_elements () =
  assert (last [1; 2; 3] = Some 3);
  assert (last ["a"; "b"; "c"] = Some "c");
  assert (last [true; false; true; false] = Some false)

let test_last_two_elements () =
  assert (last [1; 2] = Some 2);
  assert (last ["first"; "second"] = Some "second")

let () =
  test_last_empty_list ();
  test_last_single_element ();
  test_last_multiple_elements ();
  test_last_two_elements ();
  print_endline "All tests passed!"
