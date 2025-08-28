open Alcotest

let test_digits () =
  check (list int) "digits of 0" [ 0 ] (Utils.digits 0);
  check (list int) "digits of 7" [ 7 ] (Utils.digits 7);
  check (list int) "digits of 42" [ 2; 4 ] (Utils.digits 42);
  check (list int) "digits of 123" [ 3; 2; 1 ] (Utils.digits 123);
  check (list int) "digits of 1001" [ 1; 0; 0; 1 ] (Utils.digits 1001)

let test_num_digits () =
  check int "num digits of 0" 1 (Utils.num_digits 0);
  check int "num digits of 7" 1 (Utils.num_digits 7);
  check int "num digits of 42" 2 (Utils.num_digits 42);
  check int "num digits of 123" 3 (Utils.num_digits 123);
  check int "num digits of 1001" 4 (Utils.num_digits 1001)

let test_digit_at () =
  check int "digit 1 of 0" 0 (Utils.digit_at 0 1);
  check int "digit 1 of 7" 7 (Utils.digit_at 7 1);
  check int "digit 1 of 42" 2 (Utils.digit_at 42 1);
  check int "digit 2 of 42" 4 (Utils.digit_at 42 2)

let test_is_palindrome () =
  check bool "0 is palindrome" true (Utils.is_palindrome 0);
  check bool "7 is palindrome" true (Utils.is_palindrome 7);
  check bool "11 is palindrome" true (Utils.is_palindrome 11);
  check bool "121 is palindrome" true (Utils.is_palindrome 121);
  check bool "12321 is palindrome" true (Utils.is_palindrome 12321);
  check bool "123 is not palindrome" false (Utils.is_palindrome 123);
  check bool "10 is not palindrome" false (Utils.is_palindrome 10)

let test_factorise_once () =
  check (option (pair int int)) "0 is factorized" None (Utils.factorise_once 0);
  check (option (pair int int)) "7 is prime" None (Utils.factorise_once 7);
  check
    (option (pair int int))
    "42 is factorized"
    (Some (21, 2))
    (Utils.factorise_once 42);
  check
    (option (pair int int))
    "123 is factorized"
    (Some (41, 3))
    (Utils.factorise_once 123);
  check
    (option (pair int int))
    "1001 is factorized"
    (Some (143, 7))
    (Utils.factorise_once 1001)

let test_prime_factorise () =
  check (list int) "prime factors of 7" [ 7 ] (Utils.prime_factorise 7);
  check (list int) "prime factors of 42" [ 2; 3; 7 ] (Utils.prime_factorise 42);
  check (list int) "prime factors of 123" [ 3; 41 ] (Utils.prime_factorise 123);
  check (list int) "prime factors of 1001" [ 7; 11; 13 ]
    (Utils.prime_factorise 1001)

let () =
  run "Utils tests"
    [
      ("digits", [ test_case "digits" `Quick test_digits ]);
      ("num_digits", [ test_case "num_digits" `Quick test_num_digits ]);
      ("digit_at", [ test_case "digit_at" `Quick test_digit_at ]);
      ("is_palindrome", [ test_case "is_palindrome" `Quick test_is_palindrome ]);
      ( "factorise_once",
        [ test_case "factorise_once" `Quick test_factorise_once ] );
      ( "prime_factorise",
        [ test_case "prime_factorise" `Quick test_prime_factorise ] );
    ]
