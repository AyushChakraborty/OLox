(lang dune 3.22)

(name olox)

(generate_opam_files true)

(source
 (github AyushChakraborty/OLox))

(authors "Ayush Chakraborty <chakrbortyayush83@gmail.com>")

(license MIT)

(package
 (name olox)
 (synopsis "Interpreter for the Lox language in OCaml")
 (depends ocaml)
 (tags
  ("interpreters", "compilers", "crafting interpreters")))
