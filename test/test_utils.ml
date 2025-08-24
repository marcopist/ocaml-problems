open Alcotest

let test_digits () =
  check (list int) "digits of 0" [ 0 ] (Utils.digits 0);
  check (list int) "digits of 7" [ 7 ] (Utils.digits 7);
  check (list int) "digits of 42" [ 2; 4 ] (Utils.digits 42);
  check (list int) "digits of 123" [ 3; 2; 1 ] (Utils.digits 123);
  check (list int) "digits of 1001" [ 1; 0; 0; 1 ] (Utils.digits 1001)

let test_is_palindrome () =
  check bool "0 is palindrome" true (Utils.is_palindrome 0);
  check bool "7 is palindrome" true (Utils.is_palindrome 7);
  check bool "11 is palindrome" true (Utils.is_palindrome 11);
  check bool "121 is palindrome" true (Utils.is_palindrome 121);
  check bool "12321 is palindrome" true (Utils.is_palindrome 12321);
  check bool "123 is not palindrome" false (Utils.is_palindrome 123);
  check bool "10 is not palindrome" false (Utils.is_palindrome 10)

let () =
  run "Utils tests"
    [
      ("digits", [ test_case "digits" `Quick test_digits ]);
      ("is_palindrome", [ test_case "is_palindrome" `Quick test_is_palindrome ]);
    ]
