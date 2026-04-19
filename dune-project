(lang dune 3.0)
(name gadt_playground)
(generate_opam_files)

(package
 (name gadt_playground)
 (synopsis "Playground for experimenting with GADTs in OCaml")
 (description "A small OCaml project exploring GADTs, typed expressions, etc.")
 (depends
  (ocaml (>= 4.14))
  (dune (>= 3.0))
  alcotest
  (qcheck-core (>= 0.91))
  (qcheck-alcotest (>= 0.91))))
