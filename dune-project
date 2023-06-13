(lang dune 2.7)
(name zarith)

(generate_opam_files true)

(authors
  "Antoine Miné"
  "Xavier Leroy"
  "Pascal Cuoq"
)
(maintainers "Xavier Leroy <xavier.leroy@inria.fr>")
(homepage "https://github.com/ocaml/Zarith")
(bug_reports "https://github.com/ocaml/Zarith/issues")
(source
  (uri "git+https://github.com/ocaml/Zarith.git"))
(license "LGPL-2.0-only WITH OCaml-LGPL-linking-exception")

(package
 (name zarith)
 (synopsis "Implements arithmetic and logical operations over arbitrary-precision integers")
 (description
 "The Zarith library implements arithmetic and logical operations over
arbitrary-precision integers. It uses GMP to efficiently implement
arithmetic over big integers. Small integers are represented as Caml
unboxed integers, for speed and space economy.")
 (depends
   (ocaml (>= 4.04))
   (num :with-test)
   (dune (< 3.9))
   dune-configurator
   conf-gmp))
