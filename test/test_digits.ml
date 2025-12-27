open Alcotest

let test_digits () =
  let open Digits in
  let split_digits x = to_list @@ of_int x in
  check (list int) "digits of 0" [ 0 ] (split_digits 0);
  check (list int) "digits of 21" [ 2; 1 ] (split_digits 21)

let to_int_of_int_inverse =
  QCheck.Test.make ~count:1000 ~name:"to_int_of_int_inverse"
    QCheck.(0 -- 10000)
    (fun n -> Digits.to_int (Digits.of_int n) = n)

let test_sum =
  QCheck.Test.make ~count:1000 ~name:"digits_sum"
    QCheck.(pair (0 -- 10000) (0 -- 10000))
    (fun (a, b) ->
      let d1 = Digits.of_int a in
      let d2 = Digits.of_int b in
      Digits.( + ) d1 d2 = Digits.of_int (a + b))

let test_prod =
  QCheck.Test.make ~count:1000 ~name:"digits_prod"
    QCheck.(pair (0 -- 10000) (0 -- 10000))
    (fun (a, b) ->
      let d1 = Digits.of_int a in
      let d2 = Digits.of_int b in
      Digits.( * ) d1 d2 = Digits.of_int (a + b))

let () =
  run "Utils tests"
    [
      ("digits", [ test_case "digits" `Quick test_digits ]);
      ( "invariants",
        [
          QCheck_alcotest.to_alcotest to_int_of_int_inverse;
          QCheck_alcotest.to_alcotest test_sum;
          QCheck_alcotest.to_alcotest test_prod;
        ] );
    ]
