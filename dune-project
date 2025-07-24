(lang dune 3.19)

(name ocaml-problems)

(generate_opam_files true)

(source
 (github marcopist/ocaml-problems))

(authors "Marco Pistilli <pistilli.mrc@gmail.com>")

(maintainers "Marco Pistilli <pistilli.mrc@gmail.com>")

(license LICENSE)

(package
 (name ocaml-problems)
 (synopsis "OCaml Problems")
 (description "A collection of OCaml programming problems and solutions.")
 (depends ocaml alcotest fmt)
 (depopts)
 (allow_empty)
 (tags
  ("ocaml" "problems" "solutions")))
