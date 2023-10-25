(lang dune 3.11)

(name zcc)

(generate_opam_files true)

(source
 (github zdzoz/zcc))

(authors "Oskar Zdziarski")

(license MIT)

(package
 (name zcc)
 (synopsis "An educational C compiler written in OCaml")
 (depends ocaml dune)
 (tags
  (topics "c, compiler" your project)))
